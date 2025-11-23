# Whisper Model Guide

## Quick Recommendations

### For Live Transcription (whisper-live)
- **Best choice**: `base` - Fast (3-5 sec/chunk), good quality
- **Faster**: `tiny` - Very fast (1-2 sec/chunk), acceptable quality
- **Better quality**: `small` - Slower (10-15 sec/chunk), very good quality

### For Maximum Accuracy (offline)
- **Best**: `large-v3-turbo` - Slow (60-120 sec/chunk), excellent quality
- **Alternative**: `medium` - Moderate (30-45 sec/chunk), excellent quality

## Model Sizes & Performance

| Model | Size | CPU Time* | Quality | RAM |
|-------|------|-----------|---------|-----|
| tiny | 75 MB | 1-2 sec | ★★☆☆☆ | ~1 GB |
| tiny.en | 75 MB | 1-2 sec | ★★☆☆☆ | ~1 GB |
| base | 142 MB | 3-5 sec | ★★★☆☆ | ~1 GB |
| base.en | 142 MB | 3-5 sec | ★★★☆☆ | ~1 GB |
| small | 466 MB | 10-15 sec | ★★★★☆ | ~2 GB |
| small.en | 466 MB | 10-15 sec | ★★★★☆ | ~2 GB |
| medium | 1.5 GB | 30-45 sec | ★★★★★ | ~5 GB |
| medium.en | 1.5 GB | 30-45 sec | ★★★★★ | ~5 GB |
| large-v3-turbo | 1.6 GB | 60-120 sec | ★★★★★ | ~8 GB |

*CPU time for 3-second audio chunk on typical 4-core CPU

## Variants Explained

### Language-Specific Models
- **Standard** (e.g., `base`): Multilingual, supports 99 languages
- **.en** (e.g., `base.en`): English-only, ~5% faster, slightly better for English

### Quantized Models (Smaller Size)
- **Full precision** (e.g., `base`): Original quality
- **-q5_0** (e.g., `base-q5_0`): ~50% smaller, <5% quality loss
- **-q8_0** (e.g., `base-q8_0`): ~30% smaller, <2% quality loss

Example: `base-q5_0` = 71 MB instead of 142 MB, nearly same quality

## Switching Models

### Quick Switch (for testing)

```bash
# Download a faster model
cd ~/.emacs.d/.cache/whisper.cpp
bash ./models/download-ggml-model.sh base

# List downloaded models
ls -lh models/*.bin
```

### Update Your Config

Edit `~/.dotfiles/.emacs.d/inits/90-mine-000-emacs-packages.el`:

```elisp
;; Change from:
(setq whisper-model "large-v3-turbo")

;; To one of:
(setq whisper-model "base")        ; Fast, good quality (RECOMMENDED)
(setq whisper-model "base.en")     ; English-only, slightly faster
(setq whisper-model "base-q5_0")   ; Smaller file, nearly same quality
(setq whisper-model "tiny")        ; Ultra-fast, lower quality
(setq whisper-model "small")       ; Slower but better quality
```

Then reload Emacs or evaluate the config.

## Download Commands

```bash
cd ~/.emacs.d/.cache/whisper.cpp

# Fast models (recommended for live transcription)
bash ./models/download-ggml-model.sh tiny
bash ./models/download-ggml-model.sh base
bash ./models/download-ggml-model.sh base.en

# Better quality (slower)
bash ./models/download-ggml-model.sh small
bash ./models/download-ggml-model.sh small.en

# Quantized (smaller files)
bash ./models/download-ggml-model.sh base-q5_0
bash ./models/download-ggml-model.sh small-q5_0

# Maximum quality (very slow)
bash ./models/download-ggml-model.sh medium
bash ./models/download-ggml-model.sh large-v3-turbo  # Already have this
```

## Testing Different Models

```bash
# Test with sample audio
TEST_AUDIO="/tmp/test.wav"

# Record 3 seconds
ffmpeg -f pulse -i RDPSource -t 3 -ar 16000 "$TEST_AUDIO" -y

# Test different models
for model in tiny base small; do
    echo "Testing $model..."
    time ~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli \
        -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-${model}.bin \
        -f "$TEST_AUDIO" -t 4 -l en --no-timestamps
done
```

## Performance Tips

### Speed Up Transcription
1. Use smaller model: `base` or `tiny`
2. Use English-only: `base.en` instead of `base`
3. Use quantized: `base-q5_0` instead of `base`
4. Reduce threads if CPU overheats: `(setq whisper-use-threads 2)`

### Improve Quality
1. Use larger model: `small` or `medium`
2. Increase chunk duration: `(setq whisper-live-chunk-duration 5)`
3. Use initial prompt for domain vocabulary

## Summary

**For most users**: Use `base` model
- Downloads in seconds
- Transcribes 3-sec chunks in 3-5 seconds
- Good enough quality for live transcription
- Much better user experience than waiting 60+ seconds per chunk

**Current setup**: `large-v3-turbo` is overkill for live transcription
- Best for: Final transcripts, important meetings, maximum accuracy
- Too slow for: Real-time typing, interactive use, casual notes

See also: [CONFIGURATION.md](CONFIGURATION.md) for other settings
