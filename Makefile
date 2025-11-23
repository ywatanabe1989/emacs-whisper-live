# Makefile for whisper-live.el
# Author: ywatanabe@scitex.ai
# Makes the repository organized and easy to use

# Use bash for shell commands (needed for 'read -n' support)
SHELL := /bin/bash

# Configuration
WHISPER_DIR := $(HOME)/.emacs.d/.cache/whisper.cpp
WHISPER_BIN := $(WHISPER_DIR)/build/bin/whisper-cli
MODEL_DIR := $(WHISPER_DIR)/models
EMACS := emacs
MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
SCRIPTS_DIR := $(MAKEFILE_DIR)scripts

# Available models
MODELS := tiny tiny.en base base.en small small.en medium medium.en large-v1 large-v2 large-v3 large-v3-turbo

.PHONY: help setup install build download-models test clean clean-chunks uninstall \
        gpu-check gpu-setup gpu-rebuild docs status quick-setup record-test

# Default target - show help
.DEFAULT_GOAL := help

help:
	@echo "╔════════════════════════════════════════════════════════════════╗"
	@echo "║              whisper-live.el - Quick Reference                 ║"
	@echo "╚════════════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "🚀 FIRST TIME SETUP (run this first!):"
	@echo "  make quick-setup        - Interactive setup script (RECOMMENDED)"
	@echo "  make setup              - Full automated setup (install + models + test)"
	@echo ""
	@echo "📦 INSTALLATION:"
	@echo "  make install            - Install whisper.cpp (CPU only)"
	@echo "  make build              - Rebuild whisper.cpp"
	@echo "  make gpu-check          - Check if GPU acceleration is available"
	@echo "  make gpu-setup          - Setup GPU acceleration (AMD ROCm)"
	@echo "  make gpu-rebuild        - Rebuild whisper.cpp with GPU support"
	@echo ""
	@echo "🎯 MODELS (faster models = lower quality):"
	@echo "  make download-models    - Download recommended models (base, small, large)"
	@echo "  make download-tiny      - Download tiny model (fastest, ~3-5 sec/chunk)"
	@echo "  make download-base      - Download base model (balanced, ~6-10 sec/chunk)"
	@echo "  make download-small     - Download small model (good quality, ~15-20 sec)"
	@echo "  make download-quantized - Show quantized model options (smaller files)"
	@echo "  make list-models        - Show downloaded models with sizes"
	@echo ""
	@echo "🧪 TESTING:"
	@echo "  make test               - Test whisper installation (auto-detect model)"
	@echo "  make test MODEL=tiny    - Test with specific model"
	@echo "  make test-all           - Run all tests (unit + integration, ~2-3 min)"
	@echo "  make test-unit          - Run unit tests only (fast, < 1 sec)"
	@echo "  make test-integration   - Run integration tests (requires whisper.cpp)"
	@echo "  make test-fast          - Run fast tests (excludes slow tests)"
	@echo "  make test-audio         - Test audio recording (3 seconds)"
	@echo "  make test-setup         - Run comprehensive setup test"
	@echo "  make record-test        - Record and transcribe test audio"
	@echo ""
	@echo "📚 DOCUMENTATION:"
	@echo "  make docs               - Open documentation index"
	@echo "  make doc-install        - View installation guide"
	@echo "  make doc-models         - View model comparison guide"
	@echo "  make doc-gpu            - View GPU setup guide"
	@echo "  make doc-config         - View configuration reference"
	@echo "  make doc-trouble        - View troubleshooting guide"
	@echo ""
	@echo "🔧 MAINTENANCE:"
	@echo "  make status             - Show current status and configuration"
	@echo "  make clean-chunks       - Clean temporary audio files"
	@echo "  make clean              - Remove build artifacts"
	@echo "  make uninstall          - Remove whisper.cpp completely"
	@echo ""
	@echo "💡 COMMON WORKFLOWS:"
	@echo "  First time:     make quick-setup"
	@echo "  Switch model:   make download-base && update Emacs config"
	@echo "  Enable GPU:     make gpu-check && make gpu-setup"
	@echo "  Having issues:  make status && make doc-trouble"
	@echo ""
	@echo "Available models: tiny, base, small, medium, large-v3-turbo"
	@echo ""

# Quick setup - runs interactive script
quick-setup:
	@echo "Running interactive setup..."
	@bash $(SCRIPTS_DIR)/setup.sh

# Complete automated setup
setup: install download-models test
	@echo ""
	@echo "╔════════════════════════════════════════╗"
	@echo "║  ✓ Setup Complete!                     ║"
	@echo "╚════════════════════════════════════════╝"
	@echo ""
	@echo "Next steps:"
	@echo "  1. Add whisper-live config to your Emacs init file"
	@echo "  2. Restart Emacs"
	@echo "  3. Press M-RET to start transcription"
	@echo ""
	@echo "See: make doc-install for configuration example"
	@echo ""

# Show current status
status:
	@echo "╔════════════════════════════════════════╗"
	@echo "║  whisper-live Status                   ║"
	@echo "╚════════════════════════════════════════╝"
	@echo ""
	@echo "Installation:"
	@printf "  whisper.cpp:  "
	@[ -f '$(WHISPER_BIN)' ] && echo "✓ Installed" || echo "✗ Not installed (run 'make install')"
	@printf "  Acceleration: "
	@if [ -f '$(WHISPER_BIN)' ]; then \
		if ldd $(WHISPER_BIN) 2>/dev/null | grep -q libOpenCL; then \
			echo "✓ GPU (OpenCL)"; \
		elif ldd $(WHISPER_BIN) 2>/dev/null | grep -q libvulkan; then \
			echo "✓ GPU (Vulkan)"; \
		elif ldd $(WHISPER_BIN) 2>/dev/null | grep -q libopenblas; then \
			echo "✓ OpenBLAS (CPU, 2-3x faster)"; \
		else \
			echo "CPU only (run 'make gpu-check' for setup)"; \
		fi; \
	else \
		echo "N/A"; \
	fi
	@echo ""
	@echo "Models:"
	@if [ -d "$(MODEL_DIR)" ]; then \
		ls -lh $(MODEL_DIR)/ggml-*.bin 2>/dev/null | awk '{print "  " $$9 " (" $$5 ")"}' | sed 's|.*/ggml-||' | sed 's|\.bin||' || echo "  No models downloaded (run 'make download-models')"; \
	else \
		echo "  No models (run 'make download-models')"; \
	fi
	@echo ""
	@echo "Audio chunks:"
	@if [ -d "/tmp/whisper-live-chunks" ]; then \
		echo "  $$(du -sh /tmp/whisper-live-chunks 2>/dev/null | cut -f1) in /tmp/whisper-live-chunks (run 'make clean-chunks' to clean)"; \
	else \
		echo "  None"; \
	fi
	@echo ""
	@echo "Run 'make help' to see all available commands"
	@echo ""

# Install whisper.cpp
install:
	@echo "Installing whisper.cpp..."
	@if [ ! -d "$(WHISPER_DIR)" ]; then \
		mkdir -p $(HOME)/.emacs.d/.cache; \
		git clone https://github.com/ggerganov/whisper.cpp.git $(WHISPER_DIR); \
	else \
		echo "whisper.cpp already cloned"; \
	fi
	$(MAKE) build

# Build whisper.cpp
build:
	@echo "Building whisper.cpp..."
	cd $(WHISPER_DIR) && cmake -B build
	cd $(WHISPER_DIR) && cmake --build build --config Release -j$$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
	@echo "✓ Build complete"

# Download a specific model
download-model:
	@if [ -z "$(MODEL)" ]; then \
		echo "Error: MODEL not specified"; \
		echo "Usage: make download-model MODEL=large-v3-turbo"; \
		exit 1; \
	fi
	@echo "Downloading model: $(MODEL)..."
	cd $(WHISPER_DIR) && bash ./models/download-ggml-model.sh $(MODEL)

# Download commonly used models
download-models:
	@echo "Downloading recommended models..."
	$(MAKE) download-model MODEL=base
	$(MAKE) download-model MODEL=small
	$(MAKE) download-model MODEL=large-v3-turbo

# Quick model downloads
download-tiny:
	$(MAKE) download-model MODEL=tiny

download-base:
	$(MAKE) download-model MODEL=base

download-small:
	$(MAKE) download-model MODEL=small

download-large:
	$(MAKE) download-model MODEL=large-v3-turbo

# Test installation (optionally specify MODEL=tiny/base/small/large-v3-turbo)
test:
	@echo "Testing whisper installation..."
	@if [ ! -f "$(WHISPER_BIN)" ]; then \
		echo "Error: whisper-cli not found. Run 'make install' first"; \
		exit 1; \
	fi
	@if [ ! -f "$(WHISPER_DIR)/samples/jfk.wav" ]; then \
		echo "Warning: Test audio file not found"; \
		exit 0; \
	fi
	@if [ -n "$(MODEL)" ]; then \
		echo "Testing with model: $(MODEL)"; \
		$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-$(MODEL).bin -f $(WHISPER_DIR)/samples/jfk.wav -t 4 -l en; \
	else \
		echo "Running test transcription (trying available models)..."; \
		$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-tiny.bin -f $(WHISPER_DIR)/samples/jfk.wav -t 4 -l en --no-timestamps 2>/dev/null || \
		$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-base.bin -f $(WHISPER_DIR)/samples/jfk.wav -t 4 -l en --no-timestamps 2>/dev/null || \
		$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-small.bin -f $(WHISPER_DIR)/samples/jfk.wav -t 4 -l en --no-timestamps 2>/dev/null || \
		echo "Warning: No models found. Run 'make download-base' first"; \
	fi

# Transcribe a specific file
test-transcribe:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE not specified"; \
		echo "Usage: make test-transcribe FILE=/path/to/audio.wav"; \
		exit 1; \
	fi
	@if [ ! -f "$(FILE)" ]; then \
		echo "Error: File not found: $(FILE)"; \
		exit 1; \
	fi
	$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-large-v3-turbo.bin -f $(FILE) -t 4 -l en --no-timestamps

# Run comprehensive ERT test suite
test-all:
	@echo "Running all tests (unit + integration)..."
	@./scripts/run-tests.sh all

# Run only unit tests (fast)
test-unit:
	@echo "Running unit tests only..."
	@./scripts/run-tests.sh unit

# Run only integration tests (slower, requires whisper.cpp)
test-integration:
	@echo "Running integration tests only..."
	@./scripts/run-tests.sh integration

# Run fast tests only (excludes slow integration tests)
test-fast:
	@echo "Running fast tests only..."
	@./scripts/run-tests.sh fast

# Show current configuration
show-config:
	@echo "Current Configuration:"
	@echo "  Whisper directory: $(WHISPER_DIR)"
	@echo "  Whisper binary:    $(WHISPER_BIN)"
	@echo "  Model directory:   $(MODEL_DIR)"
	@echo "  Binary exists:     $$([ -f '$(WHISPER_BIN)' ] && echo 'Yes ✓' || echo 'No ✗')"

# List downloaded models
list-models:
	@echo "Downloaded models:"
	@if [ -d "$(MODEL_DIR)" ]; then \
		ls -lh $(MODEL_DIR)/ggml-*.bin 2>/dev/null | awk '{print "  " $$9 " (" $$5 ")"}' || echo "  No models found"; \
	else \
		echo "  Model directory does not exist"; \
	fi

# Clean temporary audio chunks
clean-chunks:
	@echo "Cleaning temporary audio chunks..."
	rm -rf /tmp/whisper-live-chunks/
	@echo "✓ Chunks cleaned"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@if [ -d "$(WHISPER_DIR)/build" ]; then \
		rm -rf $(WHISPER_DIR)/build; \
		echo "✓ Build artifacts removed"; \
	else \
		echo "Nothing to clean"; \
	fi

# Uninstall whisper.cpp completely
uninstall:
	@echo "This will remove whisper.cpp and all downloaded models."
	@read -p "Are you sure? [y/N] " -n 1 -r; \
	echo; \
	if [ "$$REPLY" = "y" ] || [ "$$REPLY" = "Y" ]; then \
		rm -rf $(WHISPER_DIR); \
		echo "✓ whisper.cpp uninstalled"; \
	else \
		echo "Cancelled"; \
	fi

# Development targets
.PHONY: reload-emacs test-elisp

# Reload whisper-live in Emacs
reload-emacs:
	$(EMACS) --batch --eval "(progn \
		(when (featurep 'whisper-live) (unload-feature 'whisper-live t)) \
		(add-to-list 'load-path \"$(shell pwd)\") \
		(require 'whisper-live) \
		(message \"whisper-live reloaded\"))"

# Run elisp tests
test-elisp:
	@if [ -f "run_tests.sh" ]; then \
		./run_tests.sh; \
	else \
		echo "No test script found"; \
	fi

# ═══════════════════════════════════════════════════════════════
# GPU ACCELERATION
# ═══════════════════════════════════════════════════════════════

gpu-check:
	@echo "Checking GPU readiness..."
	@bash $(SCRIPTS_DIR)/check-gpu-ready.sh

gpu-setup:
	@echo "╔════════════════════════════════════════╗"
	@echo "║  GPU Setup for AMD Radeon              ║"
	@echo "╚════════════════════════════════════════╝"
	@echo ""
	@echo "This will install AMD ROCm for GPU acceleration."
	@echo ""
	@echo "Documentation:"
	@echo "  https://rocm.docs.amd.com/projects/radeon-ryzen/en/latest/docs/install/installrad/wsl/install-radeon.html"
	@echo ""
	@echo "Steps:"
	@echo "  1. Install amdgpu-install package"
	@echo "  2. Run: sudo amdgpu-install -y --usecase=wsl,rocm --no-dkms"
	@echo "  3. Verify with: rocminfo"
	@echo "  4. Rebuild whisper.cpp: make gpu-rebuild"
	@echo ""
	@read -p "Continue with GPU setup? [y/N] " -n 1 -r; \
	echo; \
	if [ "$$REPLY" = "y" ] || [ "$$REPLY" = "Y" ]; then \
		echo "Installing AMD GPU drivers..."; \
		if [ -f /tmp/amdgpu-install.deb ]; then \
			sudo apt install -y /tmp/amdgpu-install.deb; \
		else \
			wget https://repo.radeon.com/amdgpu-install/6.4.2.1/ubuntu/jammy/amdgpu-install_6.4.60402-1_all.deb -O /tmp/amdgpu-install.deb; \
			sudo apt install -y /tmp/amdgpu-install.deb; \
		fi; \
		echo ""; \
		echo "Now run: sudo amdgpu-install -y --usecase=wsl,rocm --no-dkms"; \
		echo "This will take 5-10 minutes..."; \
	else \
		echo "Cancelled. See: make doc-gpu for manual instructions"; \
	fi

gpu-rebuild:
	@echo "Rebuilding whisper.cpp with GPU support..."
	@if [ ! -d "$(WHISPER_DIR)" ]; then \
		echo "Error: whisper.cpp not installed. Run 'make install' first"; \
		exit 1; \
	fi
	@echo "Backing up CPU build..."
	@if [ -d "$(WHISPER_DIR)/build" ]; then \
		mv $(WHISPER_DIR)/build $(WHISPER_DIR)/build.cpu-backup-$$(date +%Y%m%d-%H%M%S); \
	fi
	@echo "Detecting available GPU backends..."
	@if pkg-config --exists OpenCL 2>/dev/null; then \
		echo "  ✓ OpenCL dev libraries found"; \
		echo "Building with OpenCL + Vulkan support..."; \
		cd $(WHISPER_DIR) && cmake -B build -DGGML_OPENCL=ON -DGGML_VULKAN=ON -DCMAKE_BUILD_TYPE=Release; \
	elif [ -f /usr/lib/x86_64-linux-gnu/libvulkan.so ] || [ -f /usr/lib/libvulkan.so ]; then \
		echo "  ⚠ OpenCL dev libraries not found (install opencl-headers and ocl-icd-opencl-dev for OpenCL support)"; \
		echo "  ✓ Vulkan found"; \
		echo "Building with Vulkan-only support..."; \
		cd $(WHISPER_DIR) && cmake -B build -DGGML_VULKAN=ON -DCMAKE_BUILD_TYPE=Release; \
	else \
		echo "  ✗ No GPU backend libraries found"; \
		echo ""; \
		echo "Install GPU support:"; \
		echo "  For Vulkan: sudo apt-get install libvulkan-dev"; \
		echo "  For OpenCL: sudo apt-get install opencl-headers ocl-icd-opencl-dev"; \
		exit 1; \
	fi
	cd $(WHISPER_DIR) && cmake --build build --config Release -j$$(nproc)
	@echo ""
	@echo "╔════════════════════════════════════════╗"
	@echo "║  ✓ GPU Build Complete!                 ║"
	@echo "╚════════════════════════════════════════╝"
	@echo ""
	@echo "Verify GPU is detected:"
	@echo "  $(WHISPER_BIN) --help | grep -i gpu"
	@echo ""

# ═══════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════

docs:
	@cat docs/README.md

doc-install:
	@cat docs/INSTALL.md

doc-models:
	@cat docs/MODELS.md

doc-gpu:
	@cat docs/GPU_SETUP.md

doc-config:
	@cat docs/CONFIGURATION.md

doc-trouble:
	@cat docs/TROUBLESHOOTING.md

# ═══════════════════════════════════════════════════════════════
# TESTING & DEBUGGING
# ═══════════════════════════════════════════════════════════════

test-audio:
	@echo "Testing audio recording (3 seconds)..."
	@echo "Speak now!"
	@DEVICE=$$(pactl list sources short | grep -q RDPSource && echo RDPSource || echo default); \
	ffmpeg -f pulse -i $$DEVICE -t 3 -ar 16000 /tmp/whisper-test-$$(date +%s).wav -y 2>&1 | tail -5
	@echo "✓ Recording complete"

test-setup:
	@bash $(SCRIPTS_DIR)/test-setup.sh

record-test:
	@echo "Recording 3 seconds of audio and transcribing..."
	@DEVICE=$$(pactl list sources short | grep -q RDPSource && echo RDPSource || echo default); \
	TEST_FILE=/tmp/whisper-test-$$(date +%s).wav; \
	echo "Device: $$DEVICE"; \
	echo "Speak now!"; \
	ffmpeg -f pulse -i $$DEVICE -t 3 -ar 16000 $$TEST_FILE -y 2>&1 | grep -v "^  " | tail -3; \
	echo ""; \
	echo "Transcribing..."; \
	$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-base.bin -f $$TEST_FILE -t 4 -l en 2>&1 | grep "^\[00:" || \
	$(WHISPER_BIN) -m $(MODEL_DIR)/ggml-tiny.bin -f $$TEST_FILE -t 4 -l en 2>&1 | grep "^\[00:" || \
	echo "No model found. Run: make download-base"


# ═══════════════════════════════════════════════════════════════
# MODEL QUANTIZATION (smaller, faster models)
# ═══════════════════════════════════════════════════════════════

download-base-q5:
	$(MAKE) download-model MODEL=base-q5_1

download-small-q5:
	$(MAKE) download-model MODEL=small-q5_1

download-large-q5:
	$(MAKE) download-model MODEL=large-v3-turbo-q5_0

download-large-q8:
	$(MAKE) download-model MODEL=large-v3-turbo-q8_0

# Download all quantized versions of a model
download-quantized:
	@echo "Available quantized models:"
	@echo "  make download-base-q5     - base model, quantized (71MB vs 142MB)"
	@echo "  make download-small-q5    - small model, quantized (233MB vs 466MB)"
	@echo "  make download-large-q5    - large-v3-turbo, q5_0 (800MB vs 1.6GB)"
	@echo "  make download-large-q8    - large-v3-turbo, q8_0 (1.1GB vs 1.6GB)"
	@echo ""
	@echo "Quantization trade-offs:"
	@echo "  q5_0/q5_1: ~50% smaller, <5% quality loss"
	@echo "  q8_0:      ~30% smaller, <2% quality loss"
	@echo ""

