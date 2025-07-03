;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-04 08:59:57>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'whisper)

(defvar whisper-live--transcription-text ""
  "Current transcription text.")

(defcustom whisper-live-chunk-duration 5
  "Duration of each audio chunk in seconds."
  :type 'integer
  :group 'whisper)

(defvar whisper-live--current-process nil
  "Current recording process.")

(defvar whisper-live--chunks-directory nil
  "Directory to store temporary audio chunks.")

(defvar whisper-live--target-buffer nil
  "Buffer where transcribed text should be inserted.")

(defvar whisper-live--insert-marker nil
  "Marker for insertion position.")

(defvar whisper-live--insert-end-marker nil
  "Marker for end of insertion position.")

(defcustom whisper-live--max-chunks 30
  "Maximum number of transcription chunks to keep in history."
  :type 'integer
  :group 'whisper)

(defvar whisper-live-transcribe-hook nil
  "Hook run after each transcription.")

;; added but i am not sure whether this is useful than harmful

(defvar whisper-live--transcription-queue nil
  "Queue of files waiting to be transcribed.")

;; added but i am not sure whether this is useful than harmful

(defvar whisper-live--current-transcription nil
  "Currently running transcription process.")

;; Core functions

(defun whisper-live--cleanup ()
  "Clean up resources used by whisper-live."
  (when whisper-live--current-process
    (delete-process whisper-live--current-process))
  (when whisper-live--transcription-queue
    (setq whisper-live--transcription-queue nil))
  (setq whisper-live--current-process nil
        whisper-live--current-transcription nil
        whisper-live--transcription-text nil)
  (whisper-live--cleanup-markers))

;; (defun whisper-live--cleanup-markers ()
;;   "Clean up markers."
;;   (when whisper-live--insert-marker
;;     (set-marker whisper-live--insert-marker nil))
;;   (when whisper-live--insert-end-marker
;;     (set-marker whisper-live--insert-end-marker nil))
;;   (setq whisper-live--insert-marker nil
;;         whisper-live--insert-end-marker nil))

(defun whisper-live--cleanup-markers ()
  "Clean up markers."
  (when (markerp whisper-live--insert-marker)
    (set-marker whisper-live--insert-marker nil))
  (when (markerp whisper-live--insert-end-marker)
    (set-marker whisper-live--insert-end-marker nil))
  (setq whisper-live--insert-marker nil
        whisper-live--insert-end-marker nil))

(defvar whisper-live--initialized nil
  "Flag to track if whisper-live has been initialized.")

(defun whisper-live--init ()
  "Initialize whisper-live settings and directories."
  (whisper-live--generate-chunks-directory)
  (when (not whisper-live--initialized)
    ;; Initialize paths
    (setq whisper--install-path (expand-file-name
                                 "whisper.cpp/"
                                 (file-name-as-directory
                                  whisper-install-directory))
          whisper--temp-file
          (expand-file-name "emacs-whisper.wav"
                            temporary-file-directory)
          ;; Initialize process variables
          whisper--recording-process nil
          whisper--transcribing-process nil
          whisper--using-whispercpp nil
          whisper--progress-level "0"
          whisper--ffmpeg-input-file nil
          ;; Initialize buffers and markers
          whisper--point-buffer (current-buffer)
          whisper--marker (make-marker)
          ;; Initialize live transcription variables
          whisper-live--transcription-queue nil
          whisper-live--current-transcription nil
          whisper-live--transcription-text nil
          whisper-live--insert-marker (point-marker)
          whisper-live--insert-end-marker (point-marker))

    ;; Suppress warnings by setting mode line
    (when whisper-show-progress-in-mode-line
      (setq global-mode-string
            (remove '(t ,(whisper--mode-line-indicator 'recording))
                    global-mode-string)
            global-mode-string
            (remove '(t ,(whisper--mode-line-indicator 'transcribing))
                    global-mode-string)))

    (setq whisper-live--initialized t)))

(add-hook 'keyboard-quit-hook #'whisper-live--cleanup)


(provide 'whisper-live-core)

(when
    (not load-file-name)
  (message "whisper-live-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))