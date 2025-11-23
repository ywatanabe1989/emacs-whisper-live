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

(defvar whisper-live-beep-on-stop t
  "Beep when recording stops.")

(defvar whisper-live-beep-start-frequency 1000
  "Frequency in Hz for start beep (higher = more distinct).")

(defvar whisper-live-beep-chunk-frequency 750
  "Frequency in Hz for chunk beep (middle tone).")

(defvar whisper-live-beep-stop-frequency 500
  "Frequency in Hz for stop beep (lower = ending tone).")

(defvar whisper-live-buzzer-script
  (expand-file-name "../docs/to_claude/bin/general/wsl2-buzzer.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WSL2 buzzer script for different frequency beeps.")

(defvar whisper-live-beep-start-wav
  (expand-file-name "start_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for start beep.")

(defvar whisper-live-beep-during-wav
  (expand-file-name "during_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for chunk/during beep.")

(defvar whisper-live-beep-stop-wav
  (expand-file-name "stop_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for stop beep.")

(defvar whisper-live-use-wav-beeps t
  "Use WAV files for beeps instead of frequency-based beeps.")

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

(defun whisper-live--play-wav (wav-file)
  "Play a WAV file using available audio player.
WAV-FILE is the path to the WAV file to play."
  (when (and wav-file (file-exists-p wav-file))
    (cond
     ;; Try aplay (ALSA player - common on Linux/WSL)
     ((executable-find "aplay")
      (start-process "whisper-live-beep" nil "aplay" "-q" wav-file))
     ;; Try ffplay (from ffmpeg)
     ((executable-find "ffplay")
      (start-process "whisper-live-beep" nil "ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet" wav-file))
     ;; Try mpv
     ((executable-find "mpv")
      (start-process "whisper-live-beep" nil "mpv" "--no-video" "--really-quiet" wav-file))
     ;; Try afplay (macOS)
     ((executable-find "afplay")
      (start-process "whisper-live-beep" nil "afplay" wav-file))
     ;; Fallback to system beep
     (t (beep)))))

(defun whisper-live--beep (frequency &optional duration repeat-count)
  "Play a beep at FREQUENCY Hz or use WAV file if enabled.
FREQUENCY determines which beep type: start (1000), chunk (750), or stop (500).
DURATION is beep duration in milliseconds (default: 200).
REPEAT-COUNT is number of times to beep (default: 1).
If `whisper-live-use-wav-beeps' is t, plays corresponding WAV file.
Otherwise uses wsl2-buzzer.sh if available, or falls back to system beep."
  (if whisper-live-use-wav-beeps
      ;; Use WAV files based on frequency
      (let ((wav-file
             (cond
              ((= frequency whisper-live-beep-start-frequency)
               whisper-live-beep-start-wav)
              ((= frequency whisper-live-beep-chunk-frequency)
               whisper-live-beep-during-wav)
              ((= frequency whisper-live-beep-stop-frequency)
               whisper-live-beep-stop-wav)
              (t nil))))
        (if wav-file
            (whisper-live--play-wav wav-file)
          (beep)))
    ;; Use frequency-based beeps
    (let ((dur (or duration 200))
          (repeats (or repeat-count 1)))
      (if (and (file-exists-p whisper-live-buzzer-script)
               (file-executable-p whisper-live-buzzer-script))
          ;; Use wsl2-buzzer.sh for frequency control
          (start-process "whisper-live-beep" nil
                        whisper-live-buzzer-script
                        "-f" (number-to-string frequency)
                        "-d" (number-to-string dur)
                        "-r" (number-to-string repeats)
                        "-i" "0.1")
        ;; Fallback to system beep
        (dotimes (_ repeats)
          (beep)
          (when (> repeats 1)
            (sit-for 0.1)))))))

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