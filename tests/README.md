# Whisper-Live Test Suite

This directory contains the test suite for whisper-live using the [elisp-test](https://github.com/ywatanabe1989/elisp-test) framework.

## Test Structure

```
tests/
├── test-whisper-live-loadable.el    # Module loading tests
├── test-whisper-live-core.el        # Core functionality tests
├── test-whisper-live-audio.el       # Audio processing tests
├── test-whisper-live-transcribe.el  # Transcription logic tests
├── test-whisper-live-setup.el       # Installation and setup tests
└── test-whisper-live.el             # Integration tests (legacy)
```

## Running Tests

### Run All Tests

```bash
./scripts/run_tests.sh
```

### Run Specific Test File

```bash
./scripts/run_tests.sh -s tests/test-whisper-live-core.el
```

### Run with Debug Output

```bash
./scripts/run_tests.sh -d
```

### Run with Custom Timeout

```bash
./scripts/run_tests.sh --timeout 30
```

## Test Categories

### Module Loading Tests (`test-whisper-live-loadable.el`)
- Tests that all whisper-live modules can be loaded correctly
- Verifies module dependencies are satisfied

### Core Tests (`test-whisper-live-core.el`)
- Variable initialization and defaults
- Marker management
- Cleanup functions
- Initialization routines

### Audio Tests (`test-whisper-live-audio.el`)
- Directory and filename generation
- Audio chunk concatenation
- Audio filtering

### Transcription Tests (`test-whisper-live-transcribe.el`)
- Text normalization
- Delta computation between transcriptions
- Transcript cleaning
- Whisper command generation
- Queue management

### Setup Tests (`test-whisper-live-setup.el`)
- Whisper.cpp installation verification
- OpenBLAS linking verification
- FFmpeg availability
- Directory permissions

## Writing New Tests

Follow the ERT (Emacs Lisp Regression Testing) pattern:

```elisp
(ert-deftest test-my-function ()
  "Test description."
  (should (equal (my-function "input") "expected-output"))
  (should-not (null some-value))
  (should-error (my-function-that-should-error)))
```

### Test Tags

Use tags to categorize tests:

```elisp
(ert-deftest test-slow-operation ()
  "Test that takes a long time."
  :tags '(integration slow)
  ...)
```

Common tags:
- `setup` - Installation and setup tests
- `integration` - Integration tests requiring external tools
- `slow` - Tests that take significant time
- `performance` - Performance benchmark tests

### Skipping Tests Conditionally

```elisp
(ert-deftest test-requires-whisper ()
  "Test that needs whisper.cpp installed."
  (skip-unless (file-exists-p "~/.emacs.d/.cache/whisper.cpp"))
  ...)
```

## Test Reports

Test reports are generated in Org mode format with timestamps:

```
ELISP-TEST-REPORT-YYYYMMDD-HHMMSS-XX-SUCCESS-YY-TOTAL-ZZ-PERCENT.org
```

Reports include:
- Test execution time
- Success/failure counts
- Detailed results for each test
- Error messages and backtraces

## Prerequisites

1. **elisp-test framework**
   ```bash
   git clone https://github.com/ywatanabe1989/elisp-test ~/.emacs.d/lisp/elisp-test
   ```

2. **whisper.cpp** (for integration tests)
   ```bash
   ./scripts/setup.sh
   ```

3. **FFmpeg** (for audio tests)
   ```bash
   sudo apt install ffmpeg  # Ubuntu/Debian
   brew install ffmpeg      # macOS
   ```

## Continuous Integration

Tests can be run in CI environments using batch mode:

```bash
emacs -Q --batch \
  -l ert \
  -l tests/test-whisper-live.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

## Troubleshooting

### Tests Timeout
Increase timeout with `--timeout` flag:
```bash
./scripts/run_tests.sh --timeout 30
```

### Module Loading Errors
Ensure all dependencies are in load-path:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/whisper-live/src")
```

### Integration Tests Failing
Check that required external tools are installed:
- whisper.cpp binary at `~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli`
- At least one model downloaded
- FFmpeg available in PATH

## Related Documentation

- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_node/ert/)
- [elisp-test Framework](https://github.com/ywatanabe1989/elisp-test)
- [whisper-live Documentation](../README.md)
