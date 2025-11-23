#!/usr/bin/env bash
# GPU readiness checker for whisper-live
# Author: ywatanabe@scitex.ai

set -uo pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo "╔════════════════════════════════════════╗"
echo "║  GPU Acceleration Readiness Check     ║"
echo "╚════════════════════════════════════════╝"
echo ""

READY=true

# Check 1: /dev/dri/ exists
echo -e "${BLUE}[1/5]${NC} Checking GPU device access..."
if [ -d "/dev/dri" ] && [ -n "$(ls -A /dev/dri 2>/dev/null)" ]; then
    echo -e "${GREEN}✓${NC} GPU devices found:"
    ls -la /dev/dri/
else
    echo -e "${RED}✗${NC} No GPU devices found (/dev/dri/ missing)"
    echo "  → Need to enable GPU passthrough in Windows"
    echo "  → Run in PowerShell (Admin): wsl --update"
    echo "  → Then restart Windows"
    READY=false
fi
echo ""

# Check 2: OpenCL available
echo -e "${BLUE}[2/5]${NC} Checking OpenCL installation..."
if command -v clinfo &>/dev/null; then
    if clinfo --list &>/dev/null; then
        echo -e "${GREEN}✓${NC} OpenCL installed and working:"
        clinfo --list
    else
        echo -e "${YELLOW}⚠${NC} OpenCL installed but no devices found"
        echo "  → May need to install AMD OpenCL drivers"
        READY=false
    fi
else
    echo -e "${RED}✗${NC} OpenCL not installed"
    echo "  → Run: sudo apt-get install -y clinfo mesa-opencl-icd ocl-icd-opencl-dev"
    READY=false
fi
echo ""

# Check 3: Vulkan available
echo -e "${BLUE}[3/5]${NC} Checking Vulkan installation..."
if command -v vulkaninfo &>/dev/null; then
    if vulkaninfo --summary &>/dev/null; then
        echo -e "${GREEN}✓${NC} Vulkan installed and working"
        vulkaninfo --summary 2>/dev/null | head -20
    else
        echo -e "${YELLOW}⚠${NC} Vulkan installed but may not work"
    fi
else
    echo -e "${YELLOW}⚠${NC} Vulkan not installed (optional)"
    echo "  → Run: sudo apt-get install -y vulkan-tools mesa-vulkan-drivers"
fi
echo ""

# Check 4: whisper.cpp build status
echo -e "${BLUE}[4/5]${NC} Checking whisper.cpp GPU support..."
WHISPER_BIN="$HOME/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli"
if [ -f "$WHISPER_BIN" ]; then
    HAS_OPENCL=$(ldd "$WHISPER_BIN" 2>/dev/null | grep -q "libOpenCL" && echo "yes" || echo "no")
    HAS_VULKAN=$(ldd "$WHISPER_BIN" 2>/dev/null | grep -q "libvulkan\|libggml-vulkan" && echo "yes" || echo "no")

    if [ "$HAS_OPENCL" = "yes" ] || [ "$HAS_VULKAN" = "yes" ]; then
        echo -e "${GREEN}✓${NC} whisper-cli built with GPU support:"
        [ "$HAS_OPENCL" = "yes" ] && echo "  - OpenCL: $(ldd "$WHISPER_BIN" 2>/dev/null | grep libOpenCL | awk '{print $1}')"
        [ "$HAS_VULKAN" = "yes" ] && echo "  - Vulkan: $(ldd "$WHISPER_BIN" 2>/dev/null | grep -E 'libvulkan|libggml-vulkan' | awk '{print $1}' | head -2)"
    else
        echo -e "${YELLOW}⚠${NC} whisper-cli exists but NOT built with GPU support"
        echo "  → Need to rebuild with: make gpu-rebuild"
        READY=false
    fi
else
    echo -e "${RED}✗${NC} whisper-cli not found"
    READY=false
fi
echo ""

# Check 5: Environment
echo -e "${BLUE}[5/5]${NC} Checking environment..."
echo "  WSL Kernel: $(uname -r)"
echo "  OS: $(cat /etc/os-release | grep "^PRETTY_NAME" | cut -d'"' -f2)"
if [ "${VK_ICD_FILENAMES:-}" ]; then
    echo -e "${GREEN}✓${NC} VK_ICD_FILENAMES set: $VK_ICD_FILENAMES"
else
    echo -e "${YELLOW}⚠${NC} VK_ICD_FILENAMES not set (may be needed for AMD)"
fi
echo ""

# Summary
echo "════════════════════════════════════════"
if $READY; then
    echo -e "${GREEN}✓ READY FOR GPU ACCELERATION!${NC}"
    echo ""
    echo "GPU acceleration is enabled and ready to use."
    echo ""
else
    echo -e "${RED}✗ NOT READY YET${NC}"
    echo ""
    echo "Most likely issue: GPU passthrough not enabled in WSL2"
    echo ""
    echo "Windows setup (PowerShell as Admin):"
    echo "  1. wsl --update"
    echo "  2. wsl --shutdown"
    echo "  3. Restart Windows"
    echo "  4. Start WSL and run this script again"
    echo ""
    echo "To rebuild with GPU support:"
    echo "  cd ~/.emacs.d/lisp/whisper-live && make gpu-rebuild"
    echo ""
    echo "See: ~/.emacs.d/lisp/whisper-live/docs/GPU_SETUP.md"
    echo ""
fi
echo "════════════════════════════════════════"
echo ""
