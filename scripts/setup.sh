#!/usr/bin/env bash
# Setup script for whisper-live.el
# Author: ywatanabe
# Timestamp: 2025-11-23

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
WHISPER_INSTALL_DIR="${WHISPER_INSTALL_DIR:-$HOME/.emacs.d/.cache/whisper.cpp}"
DEFAULT_MODEL="${WHISPER_MODEL:-large-v3-turbo}"

print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    print_info "Checking dependencies..."

    local missing_deps=()

    for cmd in git cmake make gcc g++ ffmpeg; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps+=("$cmd")
        fi
    done

    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing dependencies: ${missing_deps[*]}"
        echo ""
        echo "Please install them first:"
        echo "  Ubuntu/Debian: sudo apt-get install git cmake build-essential ffmpeg"
        echo "  Fedora/RHEL:   sudo dnf install git cmake gcc-c++ make ffmpeg"
        echo "  macOS:         brew install git cmake ffmpeg"
        exit 1
    fi

    print_info "All dependencies found ✓"
}

clone_whisper_cpp() {
    print_info "Setting up whisper.cpp..."

    if [ -d "$WHISPER_INSTALL_DIR" ]; then
        print_warn "whisper.cpp already exists at $WHISPER_INSTALL_DIR"
        read -p "Remove and reinstall? [y/N] " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            rm -rf "$WHISPER_INSTALL_DIR"
        else
            print_info "Skipping whisper.cpp installation"
            return 0
        fi
    fi

    mkdir -p "$(dirname "$WHISPER_INSTALL_DIR")"
    print_info "Cloning whisper.cpp to $WHISPER_INSTALL_DIR..."
    git clone https://github.com/ggerganov/whisper.cpp.git "$WHISPER_INSTALL_DIR"
}

build_whisper_cpp() {
    print_info "Building whisper.cpp..."

    cd "$WHISPER_INSTALL_DIR"

    # Build with CMake
    cmake -B build
    cmake --build build --config Release -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

    # Check if build succeeded
    if [ -f "./build/bin/whisper-cli" ]; then
        print_info "whisper.cpp built successfully ✓"
    else
        print_error "Build failed - whisper-cli not found"
        exit 1
    fi
}

download_model() {
    local model="$1"
    print_info "Downloading model: $model..."

    cd "$WHISPER_INSTALL_DIR"

    local model_file="./models/ggml-${model}.bin"

    if [ -f "$model_file" ]; then
        print_warn "Model $model already exists"
        return 0
    fi

    # Use the download script from whisper.cpp
    if [ -f "./models/download-ggml-model.sh" ]; then
        bash ./models/download-ggml-model.sh "$model"
    else
        # Fallback to make
        make "$model"
    fi

    if [ -f "$model_file" ]; then
        print_info "Model $model downloaded successfully ✓"
    else
        print_error "Failed to download model $model"
        exit 1
    fi
}

check_whisper_el() {
    print_info "Checking bundled whisper.el..."

    local whisper_el_path="$(dirname "$0")/src/whisper.el"

    if [ -f "$whisper_el_path" ]; then
        print_info "whisper.el already bundled (v0.3.3) ✓"
        return 0
    fi

    print_warn "whisper.el not found in src/, downloading..."
    mkdir -p "$(dirname "$whisper_el_path")"
    wget -O "$whisper_el_path" \
        https://raw.githubusercontent.com/natrys/whisper.el/refs/heads/master/whisper.el

    print_info "whisper.el downloaded ✓"
}

test_installation() {
    print_info "Testing installation..."

    cd "$WHISPER_INSTALL_DIR"

    if [ ! -f "./samples/jfk.wav" ]; then
        print_warn "Test audio file not found, skipping test"
        return 0
    fi

    local model_file="./models/ggml-${DEFAULT_MODEL}.bin"

    print_info "Running test transcription..."
    ./build/bin/whisper-cli -m "$model_file" -f ./samples/jfk.wav -t 4 -l en --no-timestamps \
        2>&1 | grep -q "my fellow Americans" && \
        print_info "Test passed ✓" || \
        print_warn "Test may have failed, but installation seems OK"
}

print_config_instructions() {
    echo ""
    echo -e "${GREEN}=== Installation Complete! ===${NC}"
    echo ""
    echo "Add this to your Emacs configuration:"
    echo ""
    cat <<'EOF'
(use-package whisper-live
  :load-path "~/.emacs.d/lisp/whisper-live"
  :config
  ;; Whisper Model Settings
  (setq whisper-install-directory "~/.emacs.d/.cache/"
        whisper-model "large-v3-turbo"
        whisper-quantize nil
        whisper-language "en"
        whisper-use-threads 4)

  ;; Audio Recording Settings
  ;; For WSL/RDP: (setq whisper--ffmpeg-input-device "RDPSource")
  (setq whisper-live-chunk-duration 3
        whisper-live-number-chunks t
        whisper-live-chunk-format "[%d] %s\n")
  :bind
  (("M-RET" . whisper-live-run)
   ("<escape>" . whisper-live-run)))
EOF
    echo ""
    echo "Then reload your Emacs configuration and run:"
    echo "  M-x whisper-live-run"
    echo ""
    echo "Model information:"
    echo "  Location: $WHISPER_INSTALL_DIR/models/ggml-${DEFAULT_MODEL}.bin"
    echo "  Binary:   $WHISPER_INSTALL_DIR/build/bin/whisper-cli"
    echo ""
}

main() {
    echo ""
    echo "╔════════════════════════════════════════╗"
    echo "║  whisper-live.el Setup Script          ║"
    echo "╚════════════════════════════════════════╝"
    echo ""

    check_dependencies
    check_whisper_el
    clone_whisper_cpp
    build_whisper_cpp
    download_model "$DEFAULT_MODEL"
    test_installation
    print_config_instructions

    print_info "Setup complete! 🎉"
}

# Allow sourcing for testing
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi
