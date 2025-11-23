# whisper-live Package Structure

## Overview

whisper-live is a **self-contained Emacs package** for real-time speech transcription. It bundles all necessary Elisp dependencies and only requires external installation of whisper.cpp (the C++ inference engine).

## Package Files

\`\`\`
whisper-live/
├── whisper-live.el          # Main package entry point (v1.0.0)
├── src/                     # Source files
│   ├── whisper.el           # Bundled dependency (v0.3.3)
│   ├── whisper-live-core.el
│   ├── whisper-live-audio.el
│   ├── whisper-live-transcribe.el
│   ├── whisper-live-llm.el
│   ├── whisper-live-vterm.el
│   └── whisper-live-run.el
├── setup.sh                 # Automated setup script
├── Makefile                 # Build automation
├── README.md                # Main documentation
├── INSTALL.md               # Installation guide
├── CONFIGURATION.md         # Configuration reference
├── QUICKREF.md              # Quick reference card
└── UPGRADE_MODELS.md        # Model upgrade guide
\`\`\`

## Dependencies

### Bundled (included)
- **whisper.el** (v0.3.3) - Base whisper interface
  - Source: https://github.com/natrys/whisper.el
  - License: MIT
  - Location: \`src/whisper.el\`

### External (must install)
- **whisper.cpp** - C++ inference engine
  - Install via: \`./setup.sh\` or \`make setup\`
  - Location: \`~/.emacs.d/.cache/whisper.cpp/\`
  
### Optional
- **Anthropic Claude API** - For LLM cleanup
  - Set \`whisper-live-clean-with-llm\` to \`t\`
  - Requires \`ANTHROPIC_API_KEY\`

## Load Path

When you add whisper-live to your load-path and require it:

\`\`\`elisp
(add-to-list 'load-path "~/.emacs.d/lisp/whisper-live")
(require 'whisper-live)
\`\`\`

It automatically:
1. Adds \`src/\` to load-path
2. Loads \`whisper.el\` (bundled)
3. Loads all whisper-live-*.el modules
4. Provides \`whisper-live\` feature

## Why Bundle whisper.el?

1. **Version control** - Ensures compatibility
2. **Simplicity** - One package to install
3. **Consistency** - Same version for all users
4. **No conflicts** - Won't interfere with system whisper.el

## Package Metadata

\`\`\`elisp
;; Package: whisper-live
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, speech, transcription
;; URL: https://github.com/ywatanabe1989/whisper-live
\`\`\`

## Installation Methods

### Method 1: Automated (Recommended)
\`\`\`bash
cd ~/.emacs.d/lisp/whisper-live
./setup.sh
\`\`\`

### Method 2: Makefile
\`\`\`bash
make setup
\`\`\`

### Method 3: Manual
See [INSTALL.md](INSTALL.md)

## Updating

### Update whisper-live
\`\`\`bash
cd ~/.emacs.d/lisp/whisper-live
git pull
\`\`\`

### Update bundled whisper.el
\`\`\`bash
cd ~/.emacs.d/lisp/whisper-live/src
wget -O whisper.el https://raw.githubusercontent.com/natrys/whisper.el/master/whisper.el
\`\`\`

### Update whisper.cpp
\`\`\`bash
cd ~/.emacs.d/.cache/whisper.cpp
git pull
make clean
make build
\`\`\`

## License

- **whisper-live**: GPL-3.0-or-later
- **Bundled whisper.el**: MIT License (Copyright 2022 Imran Khan)

See [LICENSE](LICENSE) for details.
