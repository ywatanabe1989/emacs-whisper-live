;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-25 02:34:55>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/whisper-live.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; whisper-live.el --- Real-time speech transcription with Whisper -*- lexical-binding: t; -*-

;; Author: Yusuke Watanabe <ywatanabe@scitex.ai>
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, speech, transcription
;; URL: https://github.com/ywatanabe1989/emacs-whisper-live

;; Copyright (C) 2025 Yusuke Watanabe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Real-time speech transcription in Emacs using OpenAI's Whisper model.
;; This package provides continuous, live transcription with features like:
;;
;; - Chunk-based recording and transcription
;; - Delta detection to prevent duplicate text
;; - Numbered chunks for easy reference
;; - VTerm support for terminal integration
;; - Optional LLM cleanup with Claude
;;
;; whisper-live bundles whisper.el (v0.3.3) as a dependency, so you don't
;; need to install it separately. All required files are in the src/ directory.
;;
;; Usage:
;;   M-x whisper-live-run to start/stop transcription
;;
;; See README.md for full installation and configuration instructions.

;;; Code:

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


(provide 'whisper-live)

(when
    (not load-file-name)
  (message "whisper-live.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))