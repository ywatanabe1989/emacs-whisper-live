# Troubleshooting Guide

## No Transcription Appears

### Check Debug Messages

1. Try transcription again: `M-RET` (speak) then `M-RET` (stop)
2. Check messages: `C-h e` or `M-x view-echo-area-messages`
3. Look for `[whisper-live]` debug messages

### Check FFmpeg Errors

If you see "Recording failed: exited abnormally with code 1":

1. Open `*whisper-live-ffmpeg-errors*` buffer
2. Look for FFmpeg error messages

### Common Issues

#### WSL/RDP Audio (code 1 error)

**Symptom:** `Recording failed: exited abnormally with code 1`

**Solution:** Test your audio device:

```bash
# List available devices
pactl list sources short

# Test recording (should create a file)
ffmpeg -f pulse -i default -t 1 -ar 16000 /tmp/test.wav

# If that fails, try specific device
ffmpeg -f pulse -i RDPSource -t 1 -ar 16000 /tmp/test.wav
```

If `RDPSource` works, add this to your `use-package whisper-live` `:config` section:

```elisp
;; Audio Recording Settings
(setq whisper--ffmpeg-input-device "RDPSource"  ; WSL/RDP audio device
      whisper-live-chunk-duration 3)            ; Seconds per chunk
```

#### No Audio Device

**Symptom:** FFmpeg says "no such device"

**Solution:** Install PulseAudio or fix audio configuration

```bash
# Ubuntu/Debian
sudo apt-get install pulseaudio

# Start PulseAudio
pulseaudio --start
```

#### Permission Denied

**Symptom:** FFmpeg says "permission denied"

**Solution:** Check microphone permissions

```bash
# Check if you can record
arecord -d 1 /tmp/test.wav
```

#### Non-monotonous DTS Warnings

**Symptom:** FFmpeg logs show "Non-monotonous DTS in output stream" warnings (especially with PulseAudio/RDP audio)

**Impact:** Audio files are created but may have timestamp glitches

**Solution:** Fixed in v1.0.1+ with `-use_wallclock_as_timestamps 1 -fflags +genpts` flags. Update to latest version.

## Transcription Works But Output Is Wrong

### Check Regex Pattern

The fix we applied handles whisper output format `[HH:MM:SS.mmm --> HH:MM:SS.mmm]   text`

If you see no errors but no text, check the whisper output manually:

```bash
# Find latest audio file
ls -lt /tmp/whisper-live-chunks/*/combined.wav | head -1

# Test transcription
~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli \
  -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-large-v3-turbo.bin \
  -f /path/to/combined.wav \
  -t 4 -l en
```

Look at the output format and verify it matches our regex.

## Model Issues

### Model Not Found

```bash
# Check models
ls -lh ~/.emacs.d/.cache/whisper.cpp/models/

# Download missing model
cd ~/.emacs.d/.cache/whisper.cpp
bash ./models/download-ggml-model.sh large-v3-turbo
```

### Wrong Model Path

Check your config has the correct path:

```elisp
(setq whisper-install-directory "~/.emacs.d/.cache/")
```

Note: Must end with `/` and whisper.cpp will be appended.

## Performance Issues

### Slow Transcription

- Use smaller model: `base` or `small` instead of `large-v3-turbo`
- Reduce threads: `(setq whisper-use-threads 2)`
- Try GPU acceleration (see GPU Acceleration section below)

### High CPU Usage

- Increase chunk duration: `(setq whisper-live-chunk-duration 5)`
- Use quantized model: `(setq whisper-quantize "q5_0")`

## GPU Acceleration Issues

See `docs/GPU_SETUP.md` for complete GPU setup guide.

### Quick GPU Check

```bash
# Run GPU readiness check
cd ~/.emacs.d/lisp/whisper-live
make gpu-check
```

This will diagnose your GPU setup and tell you what's needed.

### Common GPU Issues on WSL2 + AMD

**Symptom**: `make gpu-check` shows "No GPU devices found"

**For AMD GPUs on WSL2**:

1. **Check `/dev/dxg` exists** (not `/dev/dri/`):
   ```bash
   ls /dev/dxg  # Should exist
   ```

2. **Check for dzn ICD** (D3D12 Vulkan driver):
   ```bash
   ls /usr/share/vulkan/icd.d/ | grep dzn
   ```

3. **If dzn is missing**, upgrade Mesa:
   ```bash
   sudo add-apt-repository ppa:oibaf/graphics-drivers
   sudo apt update && sudo apt upgrade -y
   sudo apt install -y mesa-vulkan-drivers vulkan-tools
   ```
   Then shutdown WSL from Windows: `wsl --shutdown`

4. **Test Vulkan**:
   ```bash
   export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/dzn_icd.x86_64.json
   vulkaninfo --summary | head -80
   ```

**Reality Check**: AMD GPU on WSL2 is experimental (30-40% success rate). If it doesn't work after following GPU_SETUP.md, use CPU mode - it's more reliable.

### whisper.cpp Not Using GPU

**Check if built with GPU support**:
```bash
ldd ~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli | grep vulkan
```

Should show: `libvulkan.so.1 => /usr/lib/...`

**Rebuild with GPU**:
```bash
cd ~/.emacs.d/lisp/whisper-live
make gpu-rebuild
```

## Getting Help

If none of these solutions work:

1. Check `*Messages*` buffer for `[whisper-live]` messages
2. Check `*whisper-live-ffmpeg-errors*` buffer
3. Run test manually and share output:

```bash
# Test recording
ffmpeg -f pulse -i default -t 3 -ar 16000 /tmp/test-record.wav

# Test transcription
~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli \
  -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-large-v3-turbo.bin \
  -f /tmp/test-record.wav -t 4 -l en
```

Contact: ywatanabe@scitex.ai
