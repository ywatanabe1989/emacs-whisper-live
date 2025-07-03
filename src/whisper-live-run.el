;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-04 08:52:59>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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

(when
    (not load-file-name)
  (message "whisper-live-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))