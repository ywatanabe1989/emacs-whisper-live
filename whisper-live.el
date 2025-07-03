;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-04 08:50:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/whisper-live.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Time-stamp: <2024-12-08 18:15:00 (ywatanabe)>

(let ((src-dir (expand-file-name "src"
                                 (file-name-directory
                                  (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path src-dir))

(require 'whisper)
(require 'whisper-live-core)
(require 'whisper-live-audio)
(require 'whisper-live-vterm)
(require 'whisper-live-transcribe)
(require 'whisper-live-llm)
(require 'whisper-live-run)

(message "%s was loaded."
         (file-name-nondirectory (or load-file-name buffer-file-name)))

(message "%s was loaded."
         (file-name-nondirectory (or load-file-name buffer-file-name)))


(provide 'whisper-live)

(when
    (not load-file-name)
  (message "whisper-live.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))