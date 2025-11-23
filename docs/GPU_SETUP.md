# GPU Acceleration Setup for whisper-live

## ⚠️ **RECOMMENDED: Use OpenBLAS Instead** ⚠️

**For AMD GPU on WSL2, OpenBLAS (CPU acceleration) is the recommended approach:**
- ✅ **Simple setup** (5 minutes vs hours)
- ✅ **2-3x faster than plain CPU** (good enough for live transcription)
- ✅ **Reliable** (no GPU driver issues)
- ✅ **Already working** (no complex toolchain)

### Quick OpenBLAS Setup

```bash
# Install OpenBLAS
sudo apt-get update
sudo apt-get install -y libopenblas-dev

# Rebuild whisper.cpp with OpenBLAS
cd ~/.emacs.d/.cache/whisper.cpp
mv build build.cpu-only  # backup
cmake -B build -DGGML_BLAS=ON -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release -j$(nproc)

# Test
./build/bin/whisper-cli -m models/ggml-base.bin -f samples/jfk.wav -l en
# Look for: "whisper_backend_init: using BLAS backend"
```

**Performance**: ~2-3 seconds per 5-second audio chunk (acceptable for live use)

---

## AMD GPU on WSL2 (Not Recommended - Experimental)

**Reality Check**: AMD GPU support on WSL2 is experimental and unreliable (~30-40% success rate).

### Your GPU Specs

From Task Manager screenshot:
- **GPU**: AMD Radeon Graphics
- **Dedicated VRAM**: 512 MB (on GPU chip - LOW)
- **Shared memory**: 15.6 GB (from system RAM - EXCELLENT)
- **Total available**: ~16 GB (more than enough!)

**Verdict**: Memory is NOT the issue. The shared memory setup is good!

### Current Status: GPU Not Available in WSL2

**IMPORTANT for WSL2**: WSL2 uses `/dev/dxg` (not `/dev/dri/`) for GPU access!

```bash
# Check if WSL GPU passthrough is enabled (correct way)
ls /dev/dxg  # Should exist: /dev/dxg

# Old/wrong way (won't work on WSL2)
ls /dev/dri/  # Will NOT exist on WSL2 - this is normal!
```

If `/dev/dxg` doesn't exist, GPU passthrough is not enabled in WSL2.

## Step 1: Enable GPU in WSL2 (Windows Side)

### Prerequisites
- Windows 11 (or Windows 10 version 21H2 or later)
- AMD GPU drivers installed on Windows
- WSL2 with kernel version 5.10.43.3 or later

### Check Windows Requirements

Open **PowerShell (Admin)** on Windows and run:

```powershell
# Check WSL version
wsl --version

# Check if GPU passthrough is available
wsl --status
```

### Update WSL (if needed)

```powershell
# Update WSL to latest
wsl --update

# Set WSL2 as default
wsl --set-default-version 2

# Restart WSL
wsl --shutdown
```

### Verify GPU Access in WSL2

After updating WSL, restart Ubuntu and check:

```bash
# Check for WSL GPU passthrough device
ls /dev/dxg  # Should exist

# Check Vulkan ICD files
ls /usr/share/vulkan/icd.d/

# For AMD on WSL2, you NEED dzn_icd.x86_64.json (D3D12 bridge)
# If missing, continue to Step 2
```

**AMD + WSL2 Reality Check**: AMD GPUs on WSL2 require the `dzn` (D3D12) Vulkan driver, which may not be available in older Mesa versions. This is different from NVIDIA which has better WSL2 support.

## Step 2: Install D3D12 (dzn) Vulkan Driver for AMD on WSL2

**Critical**: AMD on WSL2 needs the `dzn` (D3D12) Vulkan ICD, not the native radeon driver!

### Check if dzn is already installed

```bash
ls /usr/share/vulkan/icd.d/ | grep dzn
```

If `dzn_icd.x86_64.json` exists, skip to Step 3. Otherwise, continue:

### Upgrade Mesa to get dzn support

Ubuntu 22.04's default Mesa (23.2) may not include dzn. Upgrade to newer Mesa:

```bash
# Add Mesa PPA (provides newer Mesa with dzn support)
sudo add-apt-repository ppa:oibaf/graphics-drivers
sudo apt update
sudo apt upgrade -y

# Install Vulkan drivers and tools
sudo apt install -y mesa-vulkan-drivers vulkan-tools

# IMPORTANT: Shutdown WSL from Windows PowerShell (Admin)
# Then run: wsl --shutdown
```

After WSL restart, verify dzn is available:

```bash
# Should now show dzn_icd.x86_64.json
ls /usr/share/vulkan/icd.d/

# Test with dzn ICD
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/dzn_icd.x86_64.json
vulkaninfo --summary | head -80
```

**Expected**: You should see a device like "Microsoft Direct3D12 (AMD Radeon Graphics)"

**If still 0 devices**: AMD GPU support in WSL2 is experimental and may not work. See "Known Limitations" section.

## Step 3: Rebuild whisper.cpp with GPU Support

### Option A: Using Makefile (Recommended)

The Makefile automatically detects available GPU backends:

```bash
cd ~/.emacs.d/lisp/whisper-live
make gpu-rebuild
```

This will:
- Build with OpenCL + Vulkan if OpenCL dev libraries are found
- Build with Vulkan-only if OpenCL dev libraries are missing
- Show helpful error messages if neither is available

### Option B: Manual Build

```bash
cd ~/.emacs.d/.cache/whisper.cpp

# Backup current build
mv build build.cpu-only

# Clean and rebuild with OpenCL/Vulkan
cmake -B build \
    -DGGML_OPENCL=ON \
    -DGGML_VULKAN=ON \
    -DCMAKE_BUILD_TYPE=Release

cmake --build build --config Release -j$(nproc)

# Test if GPU is detected
./build/bin/whisper-cli --help | grep -i gpu
```

**Note**: If OpenCL dev libraries are missing, you can build with Vulkan-only:

```bash
cmake -B build -DGGML_VULKAN=ON -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release -j$(nproc)
```

## Step 4: Test GPU Acceleration

```bash
# Record test audio
ffmpeg -f pulse -i RDPSource -t 3 -ar 16000 /tmp/test-gpu.wav -y

# Test with GPU (should show GPU device detected)
~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli \
    -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-base.bin \
    -f /tmp/test-gpu.wav \
    -t 4 -l en

# Look for: "whisper_backend_init_gpu: device 0: AMD Radeon"
# Instead of: "whisper_backend_init_gpu: no GPU found"
```

## Step 5: Benchmark (GPU vs CPU)

```bash
# Test CPU speed
time ~/.emacs.d/.cache/whisper.cpp/build.cpu-only/bin/whisper-cli \
    -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-base.bin \
    -f /tmp/test-gpu.wav -t 4 -l en --no-timestamps

# Test GPU speed
time ~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli \
    -m ~/.emacs.d/.cache/whisper.cpp/models/ggml-base.bin \
    -f /tmp/test-gpu.wav -t 4 -l en --no-timestamps

# Compare results
```

Expected improvement: 2-5x faster with GPU.

## Troubleshooting

### /dev/dxg doesn't exist (GPU passthrough not working)

**Problem**: Windows GPU passthrough not enabled.

**Solutions**:
1. Update Windows to latest version
2. Update AMD GPU drivers on Windows
3. Update WSL: `wsl --update` in PowerShell (Admin)
4. **Restart Windows** (not just WSL - full reboot)
5. Check BIOS: GPU/Graphics settings enabled

**Note**: Don't worry about `/dev/dri/` not existing - WSL2 uses `/dev/dxg` instead!

### OpenCL dev libraries missing

**Symptom**: `make gpu-rebuild` fails with "Could NOT find OpenCL (missing: OpenCL_LIBRARY OpenCL_INCLUDE_DIR)"

**Solution**: Install OpenCL development headers:

```bash
sudo apt-get install -y opencl-headers ocl-icd-opencl-dev
```

Or build with Vulkan-only (if Vulkan is available):

```bash
cd ~/.emacs.d/lisp/whisper-live
make gpu-rebuild  # Will automatically fallback to Vulkan-only
```

### vulkaninfo shows 0 devices (AMD on WSL2)

**Problem**: dzn (D3D12) Vulkan ICD missing or not working.

**Diagnosis**:
```bash
# Check which ICDs you have
ls /usr/share/vulkan/icd.d/

# Test each ICD
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/radeon_icd.x86_64.json
vulkaninfo --summary | head -50  # Will fail - native radeon doesn't work on WSL2

export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/dzn_icd.x86_64.json
vulkaninfo --summary | head -50  # Should work if dzn exists
```

**Solutions**:

1. **If dzn_icd.x86_64.json is missing**: Upgrade Mesa (see Step 2)
2. **If dzn exists but shows 0 devices**: AMD WSL2 GPU support may not be working yet
   - This is a known limitation with AMD GPUs on WSL2
   - Try updating Windows and AMD drivers
   - Consider using CPU mode or Windows native instead

### clinfo shows no devices

**Problem**: This is expected on WSL2! OpenCL doesn't work well with AMD + WSL2.

**Reality**: Focus on Vulkan (dzn) instead of OpenCL for AMD on WSL2. whisper.cpp can use Vulkan.

### whisper-cli still uses CPU

**Problem**: Binary not built with GPU support.

**Check**:
```bash
# Verify GPU support in build
ldd ~/.emacs.d/.cache/whisper.cpp/build/bin/whisper-cli | grep -i opencl

# Should show: libOpenCL.so => /usr/lib/...
```

**Fix**: Rebuild with `cmake -DGGML_OPENCL=ON`

### "whisper_backend_init_gpu: no GPU found" despite /dev/dxg existing

**Problem**: Vulkan ICD not properly configured for WSL2.

**Fix for AMD on WSL2**:
```bash
# Make sure dzn ICD exists
ls /usr/share/vulkan/icd.d/dzn_icd.x86_64.json

# Set environment variable to use dzn (NOT radeon)
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/dzn_icd.x86_64.json

# Add to ~/.bashrc to make permanent
echo 'export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/dzn_icd.x86_64.json' >> ~/.bashrc
```

**Important**: Use `dzn_icd`, not `radeon_icd` on WSL2!

## Known Limitations

### AMD + WSL2 Challenges

**Current Reality** (as of 2025):
- **AMD WSL2 GPU support is experimental** - may not work at all
- Requires dzn (D3D12 Vulkan bridge), not native radeon driver
- `/dev/dxg` exists but Vulkan may still show "0 devices"
- OpenCL generally doesn't work with AMD on WSL2
- Much less mature than NVIDIA WSL2 support

**Common Issue**: Even with `/dev/dxg` present and dzn installed, AMD GPUs may still not be exposed to Vulkan in WSL2. This is a known limitation tracked in multiple GitHub issues.

**Workarounds if GPU doesn't work**:
1. Run whisper.cpp **natively on Windows** (AMD Vulkan works reliably there)
2. Use **CPU mode** in WSL2 (slower but works)
3. Try a **real Linux environment** (dual boot or separate machine)

### Alternative: Try CPU Optimization First
If GPU setup fails or is too complex:

1. Use `tiny` model (faster than `base` on CPU)
2. Reduce threads: `(setq whisper-use-threads 2)`
3. Increase chunk duration: `(setq whisper-live-chunk-duration 5)`

These CPU optimizations might be "good enough" vs the GPU hassle.

## Performance Expectations

| Model | CPU (current) | GPU (expected) |
|-------|---------------|----------------|
| tiny | 3-5 sec | 1-2 sec |
| base | 6-10 sec | 2-3 sec |
| small | 15-20 sec | 5-8 sec |

**Reality check**: Even with GPU, live transcription will have some delay. The 3-sec chunk will still take 2-3 seconds to process.

## Summary Checklist (AMD on WSL2)

- [ ] Windows updated to latest version
- [ ] AMD drivers updated on Windows
- [ ] WSL updated: `wsl --update` in PowerShell (Admin)
- [ ] **Windows fully restarted** (not just WSL)
- [ ] `/dev/dxg` exists in WSL2 (NOT `/dev/dri/`)
- [ ] Mesa upgraded with oibaf PPA
- [ ] `dzn_icd.x86_64.json` exists in `/usr/share/vulkan/icd.d/`
- [ ] `vulkaninfo --summary` shows a device (with dzn ICD)
- [ ] `VK_ICD_FILENAMES` set to dzn ICD in ~/.bashrc
- [ ] whisper.cpp rebuilt with `-DGGML_VULKAN=ON`
- [ ] Test shows GPU detected
- [ ] Benchmark shows speedup

**Estimated time**: 2-3 hours

**Success rate**: ~30-40% (AMD + WSL2 is experimental)

**Recommendation**: **Try CPU optimizations first**. AMD GPU on WSL2 is not reliable. If you need GPU, consider running whisper.cpp natively on Windows instead.
