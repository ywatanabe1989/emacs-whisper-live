<!-- ---
!-- title: ./whisper-live/docs/demo.md
!-- author: ywatanabe
!-- date: 2024-12-08 01:24:25
!-- --- -->


# Whisper-live demo

(global-set-key (kbd "C-c C-w") #'whisper-live-run)

;; Live speech transcription w/o LLM revision
(setq whisper-live-clean-with-llm nil)
In this mode, your speech will be transcribed every 5 seconds. without any modifications. 



;; Live speech transcription w LLM revision
(setq whisper-live-clean-with-llm t)
On the other hand, if you turn on the support from a large language model, the entire output will be corrected or revised in the way you specify with the prompt.















