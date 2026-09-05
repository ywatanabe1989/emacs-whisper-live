# Whisper-Live Configuration Guide

## Quick Start

### Starting/Stopping Transcription

```
M-x whisper-live-run    (toggle on/off)
```

- When **active**: You'll see `🎤LIVE` in the mode line and get a message
- When **stopped**: The indicator disappears
- Press `C-g` (keyboard-quit) to emergency stop

### ⚠️ IMPORTANT: Prevent Audio Feedback Loops

**Problem**: If transcription output plays through speakers, the microphone picks it up and re-transcribes it infinitely.

**Solutions**:
1. **Use headphones** (recommended)
2. **Mute system output** while transcribing
3. **Use push-to-talk**: Only enable transcription when speaking, disable when done

**Example of feedback loop**:
```
You: "test transcription"
Output: "test transcription"
Mic picks up: "test transcription"
Output: "test transcription test transcription"
(repeats forever...)
```

## Features

### Incremental Chunk Numbering

Each transcription chunk is automatically numbered, making it easy to reference and edit:

**Example output:**
```
[1] This is the first sentence.
[2] Here's another chunk of text.
[3] And this is the third one.
```

**Benefits:**
- Easy to reference: "delete number 2", "edit line 1"
- Visual separation with newlines
- Better for voice-controlled editing workflows
- Helps track transcription flow

**Configuration:**

Enable/disable numbering:
```elisp
(setq whisper-live-number-chunks t)  ; Default: enabled
```

Customize format:
```elisp
;; Default format
(setq whisper-live-chunk-format "[%d] %s\n")

;; Alternative formats
(setq whisper-live-chunk-format "(%d) %s\n")     ; Parentheses
(setq whisper-live-chunk-format "%d. %s\n")      ; Period style
(setq whisper-live-chunk-format ">>> %s\n")      ; No numbers, just separator
(setq whisper-live-chunk-format "%s ")           ; Disable numbering and newlines
```

## What Was Fixed

### 1. VTerm Integration
- **Problem**: Text wasn't appearing in vterm buffers because the code tried to insert into the buffer directly
- **Fix**: Now uses `vterm-send-string` to send text to the terminal process as keystrokes
- **File**: `src/whisper-live-vterm.el`, `src/whisper-live-transcribe.el:306-308`

### 2. Text Accumulation/Repetition
- **Problem**: Transcriptions included all previous audio, causing repeated text
- **Fix**: Implemented delta-based output that only sends new text while keeping context for accuracy
- **File**: `src/whisper-live-transcribe.el:291-302`

### 3. Programming Terminology Support
- **Problem**: Technical terms and programming jargon were misrecognized
- **Fix**: Added initial prompt support to prime the model with domain-specific vocabulary
- **File**: `src/whisper-live-core.el:51-57`, `src/whisper-live-transcribe.el:15-27`

## ⚠️ IMPORTANT: Upgrade Your Model First!

If you're seeing poor accuracy, repetitions, or hallucinations, you're likely using an **old/small model** (base or small from Whisper v2).

**Quick check:** Run this in your terminal:
```bash
ls -lh ~/.emacs.d/.cache/whisper.cpp/models/ggml-*.bin
```

If you see `ggml-base.bin` or `ggml-small.bin`, **you need to upgrade!**

👉 **See [UPGRADE_MODELS.md](UPGRADE_MODELS.md) for complete upgrade guide**

## Improving Transcription Accuracy

### Upgrade to Better Models

The current setup likely uses an older/smaller Whisper model. Here's how to upgrade:

#### Option 1: Large-v3-turbo (Recommended)

Add to your Emacs config:

```elisp
(setq whisper-model "large-v3-turbo"
      whisper-quantize "q5_0")  ; Good balance of speed and quality
```

The model will be downloaded automatically when you first run whisper.

**For slower CPUs**, use more aggressive quantization:
```elisp
(setq whisper-model "large-v3-turbo"
      whisper-quantize "q4_0")  ; Faster, slight quality loss
```

#### Option 2: Large-v3 (Best Quality)

For maximum accuracy (but slower):
```elisp
(setq whisper-model "large-v3"
      whisper-quantize "q5_0")
```

### Customize Initial Prompt for Your Domain

The initial prompt helps Whisper recognize domain-specific vocabulary.

**⚠️ CRITICAL: Use comma-separated keywords, NOT full sentences!**

Full sentences will leak into your transcription output as hallucinations.

**Good (comma-separated keywords):**
```elisp
(setq whisper-live-initial-prompt
      "Emacs, elisp, Python, Django, Docker, Kubernetes, React, TypeScript,
       neural network, hippocampal, EEG, ffmpeg, API, REST, GPU, AWS")
```

**Bad (will appear in output!):**
```elisp
;; DON'T DO THIS - sentences leak into transcription
(setq whisper-live-initial-prompt
      "The speaker is discussing programming and software development.")
```

**For general use (LLM conversation, documents):**
```elisp
;; Leave empty for no domain bias
(setq whisper-live-initial-prompt "")

;; Or use general academic/professional terms
(setq whisper-live-initial-prompt
      "research, analysis, methodology, results, conclusion, hypothesis")
```

**Tips:**
- Short, nouny keywords only
- Include acronyms and technical terms
- 10-30 words is ideal
- Leave empty if you don't need domain-specific recognition

### Adjust Context Window

Control how many chunks are used for context:

```elisp
(setq whisper-live-context-chunks 3)  ; Default
```

- **Lower (1-2)**: Faster, less context, more typos
- **Higher (4-6)**: Better punctuation and flow, slightly slower

## Configuration Options

### Core Settings

```elisp
;; Chunk duration (seconds of audio per transcription)
(setq whisper-live-chunk-duration 5)  ; Default: 5 seconds

;; Number of chunks to keep for context
(setq whisper-live-context-chunks 3)  ; Default: 3

;; Maximum chunks to keep before cleanup
(setq whisper-live--max-chunks 30)  ; Default: 30

;; Initial prompt (comma-separated keywords only!)
(setq whisper-live-initial-prompt "")  ; Empty = general use

;; Chunk numbering and formatting
(setq whisper-live-number-chunks t)           ; Enable numbering
(setq whisper-live-chunk-format "[%d] %s\n")  ; Format: [1] text

;; Re-transcribe all accumulated audio and revise provisional text in place
(setq whisper-live-transcription-mode 'accumulative)
(setq whisper-live-accumulative-revise-text t)
(setq whisper-live-accumulative-max-duration 120)
(setq whisper-live-accumulative-stop-at-max-duration t)

;; Audio feedback (status buzzer)
(setq whisper-live-audio-feedback t)        ; Enable status beeps
(setq whisper-live-buzzer-frequency 8000)   ; High-pitched beep
(setq whisper-live-buzzer-volume 0.1)       ; Quiet (10%)
```

### Whisper Model Settings

```elisp
;; Model choice: tiny, base, small, medium, large, large-v3, large-v3-turbo
(setq whisper-model "large-v3-turbo")

;; Quantization: q4_0, q4_1, q5_0, q5_1, q8_0, or nil
(setq whisper-quantize "q5_0")

;; Language
(setq whisper-language "en")  ; or "auto" for automatic detection

;; Number of CPU threads
(setq whisper-use-threads 4)  ; Default: whisper.cpp default (4 max)
```

## Advanced: Faster Backends

If whisper.cpp is too slow, consider:

### faster-whisper (Python)

Typically 2-4× faster than whisper.cpp. Requires Python setup:

```bash
pip install faster-whisper
```

Then override the command function (advanced - requires custom Elisp).

### Cloud API (Highest Accuracy)

For best accuracy, use OpenAI's new transcription API:
- `gpt-4o-transcribe` (highest quality)
- `gpt-4o-mini-transcribe` (faster, cheaper)

Requires API key and custom implementation.

## Usage Tips

### Toggle Workflow

**Recommended usage pattern:**
1. Position cursor where you want text
2. `M-x whisper-live-run` to start
3. Speak your text
4. `M-x whisper-live-run` to stop (or `C-g`)
5. Review and edit

### Keybinding Suggestion

Add to your config for quick access:
```elisp
(global-set-key (kbd "C-c w") 'whisper-live-run)  ; or your preferred key
```

### In VTerm

The transcription acts like typing:
- Text appears at your cursor/prompt
- Does NOT auto-execute (no Enter pressed)
- You can edit before pressing Enter
- Use for command dictation, not full conversations

### Mode Line Indicator

Customize the indicator:
```elisp
(setq whisper-live-mode-line-indicator " [REC]")  ; or any string you like
```

### Audio Feedback (Status Buzzer)

A subtle beep plays every time a new chunk is recorded, providing audio confirmation that transcription is active. The buzzer frequency is automatically filtered out from the recording so it doesn't interfere with transcription.

**Enable/disable:**
```elisp
(setq whisper-live-audio-feedback t)  ; Default: enabled
```

**Customize buzzer:**
```elisp
;; Frequency (Hz) - higher = easier to filter, less interfering with speech
(setq whisper-live-buzzer-frequency 8000)  ; Default: 8000 Hz (high-pitched)

;; Volume (0.0 to 1.0) - lower = less intrusive
(setq whisper-live-buzzer-volume 0.1)      ; Default: 0.1 (10%)
```

**How it works:**
1. Every chunk starts with a brief (100ms) beep at the configured frequency
2. FFmpeg records the audio including the beep
3. A notch filter removes the beep frequency (±500 Hz) before transcription
4. You hear status feedback, but Whisper doesn't see it

**Common frequencies:**
- `8000` Hz - High-pitched (default, safest, easiest to filter)
- `6000` Hz - Medium-high (still safe)
- `4000` Hz - Medium (may start to interfere with sibilants)
- Lower frequencies not recommended (overlap with speech)

**Disable if:**
- You find the beep annoying
- You're using headphones and prefer visual-only feedback
- Recording in very quiet environments where the beep might leak

## Troubleshooting

### Audio feedback loop / infinite repetition
- **Symptom**: Same text repeating infinitely
- **Fix**: Mute speakers or use headphones

### No text appears in vterm
- Make sure you've reloaded the updated code: `M-x eval-buffer` or restart Emacs
- Check that vterm process is running: `M-x list-processes`

### Still getting repeated text
- Verify `whisper-live--last-clean-text` is being reset on start
- Check `whisper-live-context-chunks` isn't too high

### Poor accuracy for non-native English
- Use higher quality models: `large-v3-turbo` or `large-v3`
- Lower quantization: use `q5_0` or `q8_0` instead of `q4_0`
- Customize initial prompt with your common phrases
- Ensure good microphone quality and quiet environment

### Transcription too slow
- Use `large-v3-turbo` instead of `large-v3`
- Use more aggressive quantization: `q4_0`
- Reduce `whisper-live-context-chunks` to 1 or 2
- Consider faster-whisper backend

## Example Configurations

### For Programming/Technical Work

```elisp
(setq whisper-model "large-v3-turbo"
      whisper-quantize "q5_0"
      whisper-language "en"
      whisper-use-threads 4
      whisper-live-chunk-duration 5
      whisper-live-context-chunks 3
      whisper-live-number-chunks t
      whisper-live-chunk-format "[%d] %s\n"
      whisper-live-transcription-mode 'accumulative
      whisper-live-accumulative-revise-text t
      whisper-live-audio-feedback t
      whisper-live-buzzer-frequency 8000
      whisper-live-buzzer-volume 0.1
      whisper-live-initial-prompt
      "Emacs, elisp, Python, JavaScript, TypeScript, Docker, Kubernetes,
       AWS, API, REST, GraphQL, PostgreSQL, React, Vue, GitHub, CI/CD")
```

### For General LLM Conversation or Documents

```elisp
(setq whisper-model "large-v3-turbo"
      whisper-quantize "q5_0"
      whisper-language "en"
      whisper-use-threads 4
      whisper-live-chunk-duration 5
      whisper-live-context-chunks 3
      whisper-live-number-chunks t
      whisper-live-chunk-format "[%d] %s\n"
      whisper-live-transcription-mode 'accumulative
      whisper-live-accumulative-revise-text t
      whisper-live-audio-feedback t
      whisper-live-buzzer-frequency 8000
      whisper-live-buzzer-volume 0.1
      whisper-live-initial-prompt "")  ; Empty for no domain bias
```

## Next Steps

1. **Reload the code**: Restart Emacs or `M-x eval-buffer` on all modified files
2. **Download better model**: On first run, Emacs will prompt to download `large-v3-turbo`
3. **Customize initial prompt**: Add your specific technical vocabulary
4. **Test in vterm**: Run `M-x whisper-live-run` in a vterm buffer
5. **Adjust settings**: Fine-tune based on your CPU speed and accuracy needs
