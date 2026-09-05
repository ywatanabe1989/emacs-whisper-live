<!-- Quick Reference for whisper-live.el -->

# whisper-live Quick Reference

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `M-RET` | `whisper-live-run` | Start/stop transcription |
| `<escape>` | `whisper-live-run` | Alternative toggle |
| `C-M-j` | `whisper-live-run` | Alternative toggle |
| `C-g` | — | Emergency stop |

## Makefile Commands

### Setup & Installation
```bash
make setup              # Complete setup (first time)
make install            # Install whisper.cpp
make build              # Rebuild whisper.cpp
```

### Model Management
```bash
make download-model MODEL=large-v3-turbo
make download-models    # Download recommended models
make list-models        # Show downloaded models
```

### Testing
```bash
make test               # Test with sample audio
make test-transcribe FILE=myaudio.wav
make show-config        # Show current setup
```

### Maintenance
```bash
make clean-chunks       # Clean temp audio files
make clean              # Clean build artifacts
make uninstall          # Remove everything
```

## Common Configuration

### Minimal
```elisp
(setq whisper-install-directory "~/.emacs.d/.cache/"
      whisper-model "large-v3-turbo"
      whisper-language "en")
```

### Audio Settings
```elisp
(setq whisper-live-chunk-duration 3          ; Seconds per chunk
      whisper-live-number-chunks t           ; Enable [1], [2], [3]...
      whisper-live-chunk-format "[%d] %s\n") ; Format
```

### Revisable Accumulative Transcription
```elisp
(setq whisper-live-transcription-mode 'accumulative
      whisper-live-accumulative-revise-text t
      whisper-live-number-chunks nil)
```

### LLM Enhancement (Optional)
```elisp
(setq whisper-live-clean-with-llm t
      whisper-live-anthropic-key (getenv "ANTHROPIC_API_KEY")
      whisper-live-anthropic-engine "claude-sonnet-4-20250514")
```

## Available Models

| Model | Size | RAM | Best For |
|-------|------|-----|----------|
| tiny | 75 MB | 273 MB | Testing |
| base | 142 MB | 388 MB | Quick & light |
| small | 466 MB | 852 MB | Good balance |
| medium | 1.5 GB | 2.1 GB | High accuracy |
| large-v3-turbo | 1.6 GB | ~2 GB | **Recommended** |

## Troubleshooting

### No transcription appears
1. Check `*Messages*` buffer for errors
2. Verify model exists: `make list-models`
3. Test manually: `make test`
4. Reload Emacs configuration

### Process errors
Errors are shown as transcribed text starting with `[whisper-live ERROR]`

### Audio issues
- Check microphone permissions
- Verify FFmpeg is installed: `ffmpeg -version`
- Check input device: `ffmpeg -f pulse -i default -t 1 test.wav` (Linux)

### Build issues
- Install build tools: `sudo apt-get install build-essential cmake`
- Check CMake version: `cmake --version` (need 3.10+)

## File Locations

| Item | Path |
|------|------|
| whisper.cpp | `~/.emacs.d/.cache/whisper.cpp/` |
| Models | `~/.emacs.d/.cache/whisper.cpp/models/` |
| Binary | `~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli` |
| Temp chunks | `/tmp/whisper-live-chunks/` |

## Performance Tips

1. **Use appropriate model**
   - For LLM conversations: `large-v3-turbo` (most accurate)
   - For casual use: `small` or `base` (faster)

2. **Adjust chunk duration**
   - Longer chunks (5s): Better accuracy, slower response
   - Shorter chunks (2s): Faster response, less context

3. **Use initial prompt for domain vocabulary**
   ```elisp
   (setq whisper-live-initial-prompt
         "Emacs, elisp, Python, API, GitHub")  ; Keywords only!
   ```

4. **Thread count**
   ```elisp
   (setq whisper-use-threads 4)  ; Adjust based on CPU
   ```

## Integration Examples

### With vterm
Transcription automatically types into vterm like keystrokes.

### With LLM conversations
Use numbered chunks `[1] text` for easy reference in prompts.

### With org-mode
Perfect for capturing meeting notes or brainstorming sessions.

---

For detailed information, see:
- [README.md](README.md) - Full documentation
- [INSTALL.md](INSTALL.md) - Installation guide
- [CONFIGURATION.md](CONFIGURATION.md) - Configuration options
