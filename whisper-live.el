;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-08 18:15:00
;;; Time-stamp: <2024-12-08 18:15:00 (ywatanabe)>
;;; File: ./whisper-live/whisper-live.el


(let ((src-dir (expand-file-name "src"
                                 (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path src-dir))

(require 'whisper-live-core)
(require 'whisper-live-audio)
(require 'whisper-live-transcribe)
(require 'whisper-live-llm)
(require 'whisper-live-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

(provide 'whisper-live)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
