;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-08 18:09:39
;;; Time-stamp: <2024-12-08 19:20:40 (ywatanabe)>
;;; File: ./whisper-live/src/whisper-live-run.el


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
        (message "Stopped"))
    (whisper-live--init)
    (whisper-live--update-chunks-directory)
    (whisper-live--ensure-directory)
    (whisper-live--update-tags)
    (setq whisper-live--target-buffer (current-buffer)
          whisper-live--insert-marker (point-marker)
          whisper-live--insert-end-marker (point-marker))
    (set-marker whisper-live--insert-end-marker (point))
    (whisper-live--record-chunk)
    (message "Live transcription started")))

(provide 'whisper-live-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
