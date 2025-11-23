#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# Test runner for whisper-live using elisp-test framework

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
LOG_PATH="$SCRIPT_DIR/.$(basename "$0").log"
echo > "$LOG_PATH"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo_info() { echo -e "${NC}$1${NC}"; }
echo_success() { echo -e "${GREEN}$1${NC}"; }
echo_warning() { echo -e "${YELLOW}$1${NC}"; }
echo_error() { echo -e "${RED}$1${NC}"; }

# Configuration
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"
TESTS_DIR="$PROJECT_DIR/tests"
TEST_TIMEOUT=10
DEBUG_MODE=false
SINGLE_TEST_FILE=""

usage() {
    echo_info "Usage: $0 [OPTIONS]"
    echo_info ""
    echo_info "Options:"
    echo_info "  -t, --tests-dir DIR       Directory containing test files (default: $TESTS_DIR)"
    echo_info "  -d, --debug               Enable debug output"
    echo_info "  -s, --single FILE         Run a single test file"
    echo_info "  --elisp-test PATH         Path to elisp-test framework (default: $ELISP_TEST_PATH)"
    echo_info "  --timeout SECONDS         Test timeout in seconds (default: ${TEST_TIMEOUT}s)"
    echo_info "  -h, --help                Display this help message"
    echo_info ""
    echo_info "Examples:"
    echo_info "  $0                        # Run all tests"
    echo_info "  $0 -s tests/test-whisper-live-core.el"
    echo_info "  $0 -d                     # Run with debug output"
    echo_info "  $0 --timeout 30           # Increase timeout"
    exit 0
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--tests-dir)
            TESTS_DIR="$2"
            shift 2
            ;;
        --timeout)
            TEST_TIMEOUT="$2"
            shift 2
            ;;
        --elisp-test)
            ELISP_TEST_PATH="$2"
            shift 2
            ;;
        -d|--debug)
            DEBUG_MODE=true
            shift
            ;;
        -s|--single)
            SINGLE_TEST_FILE="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo_error "Unknown option: $1"
            usage
            ;;
    esac
done

# Check elisp-test framework exists
if [ ! -d "$ELISP_TEST_PATH" ]; then
    echo_error "Error: elisp-test framework not found at $ELISP_TEST_PATH"
    echo_info "Please install it from: https://github.com/ywatanabe1989/elisp-test"
    exit 1
fi

# Determine test target
if [ -n "$SINGLE_TEST_FILE" ]; then
    TEST_TARGET="$SINGLE_TEST_FILE"
else
    TEST_TARGET="$TESTS_DIR"
fi

# Validate target
if [ ! -e "$TEST_TARGET" ]; then
    echo_error "Error: Target '$TEST_TARGET' does not exist"
    exit 1
fi

# Function to run tests
run_tests() {
    local target="$1"

    if [ -f "$target" ]; then
        echo_info "Running single test file: $(basename "$target")..."
    elif [ -d "$target" ]; then
        echo_info "Running all tests in: $target..."
    fi

    # Build Emacs command
    local emacs_cmd="emacs -Q --batch"

    # Add load paths
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$PROJECT_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$PROJECT_DIR/src\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$TESTS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "

    # Load elisp-test framework
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "

    # Set debug mode if requested
    if $DEBUG_MODE; then
        emacs_cmd+=" --eval \"(setq debug-on-error t)\" "
        emacs_cmd+=" --eval \"(setq debug-on-signal t)\" "
    fi

    # Run tests
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$target\\\" $TEST_TIMEOUT t)\" "

    # Execute command
    if $DEBUG_MODE; then
        echo_info "Command: $emacs_cmd"
        eval $emacs_cmd 2>&1 | tee -a "$LOG_PATH"
    else
        eval $emacs_cmd >> "$LOG_PATH" 2>&1
    fi

    return ${PIPESTATUS[0]}
}

# Execute tests
echo_info "=== Running whisper-live Test Suite ==="
echo ""

run_tests "$TEST_TARGET"
exit_code=$?

echo ""

# Display results
if [ $exit_code -eq 0 ]; then
    # Find and display test report
    report_file=$(find "$PROJECT_DIR" -maxdepth 2 -mmin -1 -name "*ELISP-TEST-REPORT*" -type f | head -n 1)

    if [ -n "$report_file" ]; then
        echo_success "✓ Tests completed successfully!"
        echo_success "Report: $report_file"

        # Display brief summary if report exists
        if [ -f "$report_file" ]; then
            echo ""
            echo_info "Summary:"
            grep -E "^(\*\* |SUCCESS|FAILED)" "$report_file" | head -10
        fi
    else
        echo_success "✓ Tests completed!"
    fi

    exit 0
else
    echo_error "✗ Tests failed (exit code: $exit_code)"
    echo_error "Check log for details: $LOG_PATH"

    # Show relevant errors
    if [ -f "$LOG_PATH" ]; then
        echo ""
        echo_info "Recent errors:"
        grep -i -A 3 "error\|fail" "$LOG_PATH" | tail -20
    fi

    exit 1
fi

# EOF
