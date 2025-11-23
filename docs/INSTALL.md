<!-- ---
!-- Timestamp: 2025-11-23
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/whisper-live/INSTALL.md
!-- --- -->

# Installation Guide

## About Dependencies

**whisper-live is self-contained!** It bundles whisper.el (v0.3.3) in the `src/` directory, so you only need to:

1. Install whisper.cpp (the C++ inference engine)
2. Download speech recognition models
3. Configure Emacs

The setup script handles all of this automatically.

## Quick Install (5 minutes)

### 1. Run the setup script

```bash
cd ~/.emacs.d/lisp/whisper-live
./setup.sh
```

This will install everything automatically and show you what to add to your Emacs config.

### 2. Add to your Emacs configuration

Copy the configuration shown by the setup script to your `~/.emacs.d/init.el`

### 3. Reload Emacs and test

```elisp
M-x whisper-live-run
```

Speak into your microphone, then press `M-RET` again to stop and see the transcription!

---

## Using Makefile

If you prefer make commands:

```bash
cd ~/.emacs.d/lisp/whisper-live

# See all available commands
make help

# Complete setup
make setup

# Download a specific model
make download-model MODEL=base

# Test installation
make test

# Clean temporary files
make clean-chunks
```

---

## Troubleshooting

### No transcription appears

**Fix applied in latest version!** The regex pattern has been fixed to correctly parse whisper output.

1. Reload your Emacs configuration
2. Try again with `M-x whisper-live-run`
3. If still not working, check the `*Messages*` buffer for errors

### Errors shown as transcription

This is expected behavior! Errors and warnings are now displayed as transcribed text so you can see what went wrong.

### Missing dependencies

Install required packages:

**Ubuntu/Debian:**
```bash
sudo apt-get install git cmake build-essential ffmpeg
```

**macOS:**
```bash
brew install git cmake ffmpeg
```

**Fedora/RHEL:**
```bash
sudo dnf install git cmake gcc-c++ make ffmpeg
```

### Model not found

Download the model manually:

```bash
cd ~/.emacs.d/.cache/whisper.cpp
bash ./models/download-ggml-model.sh large-v3-turbo
```

Or using make:

```bash
cd ~/.emacs.d/lisp/whisper-live
make download-model MODEL=large-v3-turbo
```

### Build failed

Make sure you have:
- CMake 3.10 or later
- A C++11 compatible compiler (gcc 4.8+, clang 3.3+)
- Make or ninja build system

---

## Configuration Options

See [CONFIGURATION.md](CONFIGURATION.md) for detailed configuration options.

### Minimal Configuration

```elisp
(use-package whisper-live
  :load-path "~/.emacs.d/lisp/whisper-live"
  :config
  (setq whisper-install-directory "~/.emacs.d/.cache/"
        whisper-model "large-v3-turbo"
        whisper-language "en")
  :bind ("M-RET" . whisper-live-run))
```

### Recommended Configuration

See the example in [README.md](README.md) for a full configuration with all features.

---

## Verify Installation

Check that everything is installed correctly:

```bash
cd ~/.emacs.d/lisp/whisper-live
make show-config
make list-models
```

---

## Uninstall

To remove whisper-live:

```bash
cd ~/.emacs.d/lisp/whisper-live
make uninstall
```

This will remove whisper.cpp and all downloaded models. Your Emacs configuration will need to be updated manually.

<!-- EOF -->
