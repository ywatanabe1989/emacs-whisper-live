;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 06:06:44>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-transcribe.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Time-stamp: <2024-12-08 19:17:19 (ywatanabe)>

(require 'whisper-live-core)
(require 'whisper-live-audio)
(require 'whisper-live-vterm)

(defvar whisper-live--transcription-queue nil
  "Queue of files waiting to be transcribed.")

(defvar whisper-live--current-transcription nil
  "Currently running transcription process.")

(defun whisper-live--process-transcription-queue ()
  "Process next file in transcription queue if available."
  (when (and (not whisper-live--current-transcription)
             whisper-live--transcription-queue)
    (let ((next-file (pop whisper-live--transcription-queue)))
      (setq whisper-live--current-transcription
            (whisper-live--transcribe-chunk next-file)))))

(defun whisper-live--clean-transcript (text)
  "Clean transcript TEXT by removing noise and brackets."
  (when text
    (let
        ((cleaned
          (replace-regexp-in-string "\\[.*?\\]\\|([^)]*)" "" text)))
      (setq cleaned
            (replace-regexp-in-string
             "whisper_print_progress_callback: progress = +[0-9]+%" ""
             cleaned))
      (string-trim cleaned))))

(defun whisper-live--extract-text-from-output (buffer)
  "Extract transcribed text from whisper output BUFFER."
  (with-current-buffer buffer
    (when (string-match "\n\n \\(.*\\)\n\n" (buffer-string))
      (match-string 1 (buffer-string)))))

(defun whisper-live--insert-into-vterm (clean-text)
  "Insert CLEAN-TEXT into vterm buffer as numbered line."
  (when-let ((proc (get-buffer-process (current-buffer))))
    ;; Only proceed if we have meaningful text (not empty, not just whitespace/punctuation)
    (when (and (not (string-empty-p clean-text))
               (not (string-empty-p (string-trim clean-text)))
               (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
      ;; Increment counter
      (setq whisper-live--sentence-counter
            (1+ whisper-live--sentence-counter))
      ;; Send numbered text with newline
      (vterm-send-string
       (format "%d. %s\n" whisper-live--sentence-counter clean-text)))))

(defun whisper-live--insert-into-term (clean-text)
  "Insert CLEAN-TEXT into term buffer as numbered line."
  ;; Only proceed if we have meaningful text (not empty, not just whitespace/punctuation)
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    ;; Increment counter
    (setq whisper-live--sentence-counter
          (1+ whisper-live--sentence-counter))
    ;; Send numbered text with newline
    (term-send-string (get-buffer-process (current-buffer))
                      (format "%d. %s\n"
                              whisper-live--sentence-counter
                              clean-text))))

(defun whisper-live--insert-into-buffer (clean-text)
  "Insert CLEAN-TEXT into regular buffer as numbered line."
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    (let ((inhibit-read-only t))
      ;; Increment counter
      (setq whisper-live--sentence-counter
            (1+ whisper-live--sentence-counter))
      ;; Go to end marker and insert numbered text
      (goto-char whisper-live--insert-end-marker)
      (when whisper-live-clean-with-llm
        (insert whisper-live-start-tag))
      (insert
       (format "%d. %s\n" whisper-live--sentence-counter clean-text))
      (when whisper-live-clean-with-llm
        (insert whisper-live-end-tag))
      (set-marker whisper-live--insert-end-marker (point)))))

(defun whisper-live--handle-transcription (text)
  "Handle transcription TEXT by inserting into buffer."
  (when (and text
             (not whisper-live--canceling)  ; Skip if canceling
             (not (string-empty-p (string-trim text)))
             (not (string-match-p "^[[:space:].,]*$" text))
             (markerp whisper-live--insert-marker)
             (markerp whisper-live--insert-end-marker))
    (setq whisper-live--transcription-text
          (whisper-live--clean-transcript text))
    (let* ((target-buffer (marker-buffer whisper-live--insert-marker))
           (clean-text
            (whisper--live-remove-tags
             whisper-live--transcription-text)))
      (when (and (buffer-live-p target-buffer)
                 (not (string-empty-p clean-text)))
        (with-current-buffer target-buffer
          (cond
           ((derived-mode-p 'vterm-mode)
            ;; VTerm: append numbered line (uses concatenated audio)
            (whisper-live--insert-into-vterm clean-text))
           ((derived-mode-p 'term-mode)
            ;; Term: append numbered line (uses concatenated audio)
            (whisper-live--insert-into-term clean-text))
           (buffer-read-only
            (message "Read-only buffer: %s" clean-text))
           (t
            ;; Regular buffer: append numbered line (uses concatenated audio)
            (whisper-live--insert-into-buffer clean-text))))
        (run-hooks 'whisper-live-transcribe-hook)))))

(defun whisper-live--transcribe-chunk (concatenated-file)
  "Transcribe a single CONCATENATED-FILE."
  (let ((cmd (whisper-command concatenated-file))
        (temp-buffer (generate-new-buffer " *whisper-temp*"))
        (start-time (current-time)))
    (message "[whisper-live] Transcribing: %s" concatenated-file)
    (message "[whisper-live] Command: %S" cmd)
    (when (and cmd (car cmd))
      ;; Record start time
      (setq whisper-live--last-transcription-time start-time)
      ;; Kill any existing transcription process first
      (when (and whisper-live--current-transcription
                 (process-live-p whisper-live--current-transcription))
        (message
         "[whisper-live] WARNING: Killing existing transcription process!")
        (delete-process whisper-live--current-transcription))
      (setq whisper-live--current-transcription
            (make-process
             :name "whisper-live-transcribing"
             :command cmd
             :buffer temp-buffer
             :sentinel (lambda (process event)
                         (let
                             ((process-buffer (process-buffer process)))
                           (message
                            "[whisper-live] Process event: %s, status: %s"
                            event (process-status process))
                           (when (string-equal "finished\n" event)
                             ;; Calculate transcription duration
                             (let*
                                 ((duration
                                   (float-time
                                    (time-subtract (current-time)
                                                   start-time)))
                                  (text
                                   (whisper-live--extract-text-from-output
                                    process-buffer))
                                  (raw-output
                                   (with-current-buffer process-buffer
                                     (buffer-string))))
                               (setq
                                whisper-live--transcription-duration
                                duration)
                               ;; Debug: show raw output when text is empty
                               (when
                                   (or (not text)
                                       (string-empty-p
                                        (string-trim text)))
                                 (message
                                  "[whisper-live] DEBUG - Empty text! Raw output:\n%s"
                                  (substring raw-output 0
                                             (min 500
                                                  (length raw-output)))))
                               ;; Show info in messages
                               (message
                                "[whisper-live] Transcription #%d took %.2fs: %s"
                                (1+ whisper-live--sentence-counter)
                                duration
                                (if text
                                    (substring text 0
                                               (min 50 (length text)))
                                  ""))
                               (whisper-live--handle-transcription
                                text))
                             (setq whisper-live--current-transcription
                                   nil)
                             (whisper-live--process-transcription-queue)
                             ;; Only start next recording if queue is empty (back-off system)
                             (when
                                 (not
                                  whisper-live--transcription-queue)
                               (whisper-live--record-chunk))
                             (kill-buffer process-buffer)))))))))

(defun whisper-live--record-chunk ()
  "Record a single audio chunk."
  (let ((chunk-file (whisper-live--generate-chunk-filename)))
    (setq whisper-live--current-process
          (make-process
           :name "whisper-live-recording"
           :command `("ffmpeg"
                      "-f" ,whisper--ffmpeg-input-format
                      "-i" ,whisper--ffmpeg-input-device
                      "-t"
                      ,(number-to-string whisper-live-chunk-duration)
                      "-ar" "16000"
                      "-y" ,chunk-file)
           :sentinel (lambda (_process event)
                       (when (string-equal "finished\n" event)
                         (message
                          "[whisper-live] Chunk recorded, beep-on-chunk=%s"
                          whisper-live-beep-on-chunk)
                         (when whisper-live-beep-on-chunk
                           (beep)
                           (message "[whisper-live] Beep!"))
                         ;; Transcribe concatenated chunks for better context
                         (let ((combined-file
                                (whisper-live--concatenate-chunks
                                 whisper-live--chunks-directory)))
                           (message "[whisper-live] Combined file: %s"
                                    combined-file)
                           (push combined-file
                                 whisper-live--transcription-queue)
                           (whisper-live--process-transcription-queue))))))))


(provide 'whisper-live-transcribe)

(when
    (not load-file-name)
  (message "whisper-live-transcribe.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))