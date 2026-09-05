# Changelog

## 1.4.0 - 2026-09-05

- Add optional SSH-backed transcription with bundled client and worker scripts.
- Add revisable accumulative transcripts for continuous, natural prose.
- Record the next audio chunk while the previous chunk is being transcribed.
- Finish pending audio and transcription before stopping or submitting vterm input.
- Allow chunk numbering to be disabled and configure the prose separator.
- Keep the optional LLM cleanup dependency lazy so the base package loads without request.el.
- Remove obsolete delta API tests and documentation.

## 1.3.0 - 2025-07-03

- Improve accumulative transcription support and runtime defaults.
