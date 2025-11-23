;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 05:51:38>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(require 'whisper)

(defvar whisper-live--transcription-text ""
  "Current transcription text.")

(defvar whisper-live-chunk-duration 5
  "Duration of each audio chunk in seconds.")

(defvar whisper-live-beep-on-start t
  "Beep when recording starts.")

(defvar whisper-live-beep-on-chunk t
  "Beep when each chunk is recorded.")

(defvar whisper-live-display-words 15
  "Number of words to display from each transcription chunk.
Set to nil to display full text.")

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

(defvar whisper-live--max-chunks 6
  "Maximum number of transcription chunks to keep in history.
Reduced to 6 (~30 seconds of audio) to prevent whisper timeout on long audio files.")

(defvar whisper-live-transcribe-hook nil
  "Hook run after each transcription.")

(defvar whisper-live--transcription-queue nil
  "Queue of files waiting to be transcribed.")

(defvar whisper-live--current-transcription nil
  "Currently running transcription process.")

(defvar whisper-live--last-sent-length 0
  "Length of last text sent to terminal for backspacing.")

(defvar whisper-live--sentence-counter 0
  "Counter for numbering transcribed sentences.")

(defvar whisper-live--chunk-id 0
  "Incrementing ID for each transcription chunk.")

(defvar whisper-live--chunks nil
  "List of chunks as plists: (:id N :text \"...\" :raw \"...\" :time TIME).")

(defvar whisper-live--last-transcription-time nil
  "Time when last transcription started.")

(defvar whisper-live--transcription-duration 0
  "Duration of last transcription in seconds.")

(defvar whisper-live--canceling nil
  "Flag to indicate transcription is being canceled.")

;; Core functions

(defun whisper-live--cleanup (&optional canceling)
  "Clean up resources used by whisper-live.
If CANCELING is non-nil, set canceling flag to prevent insertions."
  (when canceling
    (setq whisper-live--canceling t))
  (when whisper-live--current-process
    (delete-process whisper-live--current-process))
  (when whisper-live--current-transcription
    (delete-process whisper-live--current-transcription))
  (when whisper-live--transcription-queue
    (setq whisper-live--transcription-queue nil))
  (setq whisper-live--current-process nil
        whisper-live--current-transcription nil
        whisper-live--transcription-text nil
        whisper-live--last-sent-length 0
        whisper-live--sentence-counter 0
        whisper-live--chunk-id 0
        whisper-live--chunks nil)
  (whisper-live--cleanup-markers)
  ;; Reset canceling flag after a short delay
  (when canceling
    (run-with-timer 0.1 nil
                    (lambda () (setq whisper-live--canceling nil)))))

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
          whisper-live--last-sent-length 0
          whisper-live--sentence-counter 0
          whisper-live--chunk-id 0
          whisper-live--chunks nil
          whisper-live--canceling nil
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

(defun whisper-live--cleanup-on-quit ()
  "Cleanup when user cancels with C-g."
  (when whisper-live--current-process
    (whisper-live--cleanup t)))

(add-hook 'keyboard-quit-hook #'whisper-live--cleanup-on-quit)


(provide 'whisper-live-core)

(when
    (not load-file-name)
  (message "whisper-live-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))