#!/usr/bin/env bash
# Quick test script for whisper-live setup
# Author: ywatanabe@scitex.ai

set -euo pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo ""
echo "╔════════════════════════════════════════╗"
echo "║  whisper-live Quick Test              ║"
echo "╚════════════════════════════════════════╝"
echo ""

# Test 1: Audio devices
echo -e "${YELLOW}[1/5]${NC} Checking audio devices..."
if pactl list sources short | grep -q "RDPSource\|default"; then
    echo -e "${GREEN}✓${NC} Audio devices available:"
    pactl list sources short
else
    echo -e "${RED}✗${NC} No audio devices found"
    exit 1
fi
echo ""

# Test 2: FFmpeg recording
echo -e "${YELLOW}[2/5]${NC} Testing FFmpeg recording (2 seconds)..."
AUDIO_DEVICE=$(pactl list sources short | grep -q "RDPSource" && echo "RDPSource" || echo "default")
if ffmpeg -f pulse -i "$AUDIO_DEVICE" -t 2 -ar 16000 /tmp/test-whisper.wav -y &>/dev/null; then
    SIZE=$(stat -c%s /tmp/test-whisper.wav)
    echo -e "${GREEN}✓${NC} Recording successful (${SIZE} bytes)"
    echo "  Device: $AUDIO_DEVICE"
else
    echo -e "${RED}✗${NC} Recording failed"
    exit 1
fi
echo ""

# Test 3: Whisper binary
echo -e "${YELLOW}[3/5]${NC} Checking whisper-cli binary..."
WHISPER_BIN="$HOME/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli"
if [ -x "$WHISPER_BIN" ]; then
    echo -e "${GREEN}✓${NC} Binary found: $WHISPER_BIN"
else
    echo -e "${RED}✗${NC} Binary not found or not executable"
    exit 1
fi
echo ""

# Test 4: Model
echo -e "${YELLOW}[4/5]${NC} Checking whisper model..."
MODEL_PATH="$HOME/.emacs.d/.cache/whisper.cpp/models/ggml-large-v3-turbo.bin"
if [ -f "$MODEL_PATH" ]; then
    SIZE=$(du -h "$MODEL_PATH" | cut -f1)
    echo -e "${GREEN}✓${NC} Model found: $SIZE"
else
    echo -e "${RED}✗${NC} Model not found at $MODEL_PATH"
    exit 1
fi
echo ""

# Test 5: Transcription
echo -e "${YELLOW}[5/5]${NC} Testing transcription (this may take 30-60 seconds)..."
OUTPUT=$("$WHISPER_BIN" -m "$MODEL_PATH" -f /tmp/test-whisper.wav -t 4 -l en --no-timestamps 2>&1 | grep -A1 "^\[00:" | tail -1 | xargs)
if [ -n "$OUTPUT" ]; then
    echo -e "${GREEN}✓${NC} Transcription successful!"
    echo "  Output: \"$OUTPUT\""
else
    echo -e "${YELLOW}⚠${NC} Transcription completed but output was empty (silence?)"
fi
echo ""

# Summary
echo "╔════════════════════════════════════════╗"
echo "║  All tests passed! ✓                   ║"
echo "╚════════════════════════════════════════╝"
echo ""
echo "Your audio device is: $AUDIO_DEVICE"
echo ""
echo "If using WSL/RDP with RDPSource, ensure your config has:"
echo "  (setq whisper--ffmpeg-input-device \"RDPSource\")"
echo ""
echo "Now reload your Emacs config and run: M-x whisper-live-run"
echo ""

# Cleanup
rm -f /tmp/test-whisper.wav
