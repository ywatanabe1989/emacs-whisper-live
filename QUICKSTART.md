# whisper-live Quick Start

## Forgot What to Do? Just Type:

```bash
cd ~/.emacs.d/lisp/whisper-live
make
```

This shows all available commands!

## Common Commands (Memory Aid)

### I forgot everything!
```bash
make               # Show all commands
make status        # What's installed?
make doc-trouble   # Something's broken
```

### First time setup
```bash
make quick-setup   # Interactive setup (EASIEST)
```

### Check what's going on
```bash
make status        # Current installation status
make list-models   # What models do I have?
make gpu-check     # Can I use GPU?
```

### Download faster/better models
```bash
make download-tiny   # Fastest (3-5 sec)
make download-base   # Balanced (6-10 sec)  ← RECOMMENDED
make download-small  # Better quality (15-20 sec)
```

### GPU acceleration
```bash
make gpu-check      # Can I use GPU?
make gpu-setup      # Install ROCm drivers
make gpu-rebuild    # Rebuild with GPU support
```

### Testing
```bash
make test-audio     # Can I record audio?
make record-test    # Record & transcribe test
make test-setup     # Full system check
```

### Documentation
```bash
make docs          # All docs
make doc-models    # Model comparison
make doc-gpu       # GPU setup guide
make doc-trouble   # Troubleshooting
```

### Cleanup
```bash
make clean-chunks  # Delete temp audio files
```

## Quick Workflows

### "I just cloned this, what do I do?"
```bash
make quick-setup
# Then add config to Emacs and restart
```

### "Transcription is too slow"
```bash
make download-tiny
# Update Emacs config: (setq whisper-model "tiny")
```

### "I want to try GPU"
```bash
make gpu-check
make gpu-setup
# Follow on-screen instructions
make gpu-rebuild
```

### "It's not working!"
```bash
make status         # What's installed?
make doc-trouble    # Read troubleshooting
```

## Remember

- **Just type `make`** to see all commands
- **Use `make status`** to check current state
- **Models**: tiny (fast) → base (balanced) → small (quality)
- **GPU is optional** - CPU works fine with tiny/base models

## Emacs Usage

After setup:
- `M-RET` - Start/stop transcription
- That's it!
