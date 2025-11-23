<!-- ---
!-- Timestamp: 2025-11-24 05:51:36
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/README.md
!-- --- -->

# Source Files

This directory contains the core source files for whisper-live:

## Core Components

- **whisper.el** (v0.3.3) - Base whisper interface (bundled dependency from [natrys/whisper.el](https://github.com/natrys/whisper.el))
- **whisper-live-core.el** - Core variables and initialization
- **whisper-live-audio.el** - Audio recording and chunk management
- **whisper-live-transcribe.el** - Transcription processing and delta detection
- **whisper-live-llm.el** - Optional LLM cleanup integration
- **whisper-live-vterm.el** - VTerm terminal integration
- **whisper-live-run.el** - Main entry point and toggle function

## Bundled Dependencies

### whisper.el (v0.3.3)

This is a bundled copy of [natrys/whisper.el](https://github.com/natrys/whisper.el), which provides the base interface to whisper.cpp. We bundle it to:

1. Ensure compatibility with whisper-live
2. Avoid requiring users to install it separately
3. Control the version for stability

**License:** MIT (same as whisper.cpp)
**Upstream:** https://github.com/natrys/whisper.el

To update whisper.el to the latest upstream version:

```bash
wget -O whisper.el https://raw.githubusercontent.com/natrys/whisper.el/refs/heads/master/whisper.el
```

## Load Order

When you load whisper-live, files are loaded in this order:

1. `whisper.el` - Base whisper functionality
2. `whisper-live-core.el` - Core setup
3. `whisper-live-audio.el` - Audio handling
4. `whisper-live-vterm.el` - Terminal support
5. `whisper-live-transcribe.el` - Transcription logic
6. `whisper-live-llm.el` - LLM integration
7. `whisper-live-run.el` - User commands

This order ensures all dependencies are available when needed.

<!-- EOF -->