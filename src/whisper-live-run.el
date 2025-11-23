;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 05:51:39>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Time-stamp: <2024-12-08 19:20:40 (ywatanabe)>

(require 'whisper-live-core)
(require 'whisper-live-audio)
(require 'whisper-live-transcribe)
(require 'whisper-live-llm)

;;;###autoload

(defun whisper-live-run ()
  "Toggle live transcription."
  (interactive)
  (if whisper-live--current-process
      (progn
        (whisper-live--cleanup)
        ;; Play stop beep without verbose messages
        (when whisper-live-beep-on-stop
          (whisper-live--beep whisper-live-beep-stop-frequency 300 1))
        (message "Transcription stopped"))
    ;; Always cleanup any existing processes before starting
    (when (or whisper-live--current-process
              whisper-live--current-transcription
              whisper-live--transcription-queue)
      (whisper-live--cleanup))
    (whisper-live--init)
    (whisper-live--update-chunks-directory)
    (whisper-live--ensure-directory)
    (whisper-live--update-tags)
    (setq whisper-live--target-buffer (current-buffer)
          whisper-live--insert-marker (point-marker)
          whisper-live--insert-end-marker (point-marker))
    (set-marker whisper-live--insert-end-marker (point))
    ;; Add mode line indicator
    (whisper-live--add-mode-line)
    ;; Start auto-stop timers if enabled
    (whisper-live--start-auto-stop-timers)
    (whisper-live--record-chunk)
    ;; Play start beep without verbose messages
    (when whisper-live-beep-on-start
      (whisper-live--beep whisper-live-beep-start-frequency 200 1))
    (message "Live transcription started")))


(provide 'whisper-live-run)

(when
    (not load-file-name)
  (message "whisper-live-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))