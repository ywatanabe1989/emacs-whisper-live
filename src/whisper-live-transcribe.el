;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 06:06:44>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-transcribe.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Time-stamp: <2024-12-08 19:17:19 (ywatanabe)>

(require 'cl-lib)
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
    (let ((cleaned text))
      ;; Remove whisper progress messages (can appear inline with text)
      (setq cleaned
            (replace-regexp-in-string
             "whisper_print_progress_callback: progress = +[0-9]+%?" ""
             cleaned))
      ;; Remove any whisper_ system messages that might be inline
      (setq cleaned
            (replace-regexp-in-string
             "whisper_[a-z_]+:" ""
             cleaned))
      ;; Remove [BLANK_AUDIO] and similar markers (but keep Japanese brackets like 「」)
      (setq cleaned
            (replace-regexp-in-string
             "\\[BLANK_AUDIO\\]\\|\\[_BEG_\\]\\|\\[_TT_[0-9]+\\]" ""
             cleaned))
      ;; Remove parenthetical noise markers like (background noise), (lips smacking)
      (setq cleaned
            (replace-regexp-in-string
             "(\\(?:lips smacking\\|background noise\\|inaudible\\|coughing\\|breathing\\|music\\|whimpering\\)[^)]*)" ""
             cleaned))
      ;; Remove any remaining parenthetical sound descriptions
      (setq cleaned
            (replace-regexp-in-string
             "(\\s-*)" ""  ;; Empty parentheses
             cleaned))
      ;; Remove line-start whisper messages
      (setq cleaned
            (replace-regexp-in-string
             "^whisper_.*$" ""
             cleaned))
      (string-trim cleaned))))

(defun whisper-live--extract-text-from-output (buffer)
  "Extract transcribed text from whisper output BUFFER."
  (with-current-buffer buffer
    (let ((output (buffer-string)))
      ;; Try multiple patterns to extract text
      (cond
       ;; Pattern 0: Text after language detection (best for Japanese)
       ((string-match "auto-detected language:.*?\\([a-z]+\\).*?\n\n\\([^\n]*\\)" output)
        (let ((text (match-string 2 output)))
          ;; Remove any trailing whisper_ messages
          (when (string-match "\\(.*?\\)\\(?:whisper_\\|$\\)" text)
            (string-trim (match-string 1 text)))))
       ;; Pattern 1: Standard "\n\n TEXT\n\n" format (skip system lines)
       ((string-match "\n\n[ \t]*\\(.+\\)[ \t]*\n\n" output)
        (let ((text (match-string 1 output)))
          ;; Skip if it's a system_info or main: line
          (if (or (string-match-p "^system_info:" text)
                  (string-match-p "^main:" text)
                  (string-match-p " = .* | " text))
              ;; Try to find actual transcription after system lines
              (when (string-match "main:.*\n+\\(.+\\)" output)
                (match-string 1 output))
            text)))
       ;; Pattern 2: Text after all model loading messages (more flexible)
       ((string-match "whisper_model_load:.*\n+\\(.+\\)" output)
        (let ((text (match-string 1 output)))
          ;; Clean up: take everything until next whisper_ line or end
          (when (string-match "\\(.*?\\)\\(?:\nwhisper_\\|$\\)" text)
            (string-trim (match-string 1 text)))))
       ;; Pattern 3: Any text after the model info, before process ends
       ((string-match "whisper_full_.*\n+\\([^\n]+\\)" output)
        (match-string 1 output))
       ;; Pattern 4: Last substantial line that's not a system/whisper message
       (t
        (let ((lines (split-string output "\n" t)))
          (cl-loop for line in (reverse lines)
                   when (and (not (string-match-p "^whisper_" line))
                             (not (string-match-p "^main:" line))
                             (not (string-match-p "^system_info:" line))
                             ;; Skip lines with system info pattern (contains = and |)
                             (not (string-match-p " = .* | " line))
                             (> (length (string-trim line)) 0))
                   return (string-trim line))))))))

(defun whisper-live--insert-chunk-vterm (chunk-id clean-text)
  "Insert CLEAN-TEXT into vterm buffer with CHUNK-ID."
  (when-let ((proc (get-buffer-process (current-buffer))))
    ;; Only proceed if we have meaningful text
    (when (and (not (string-empty-p clean-text))
               (not (string-empty-p (string-trim clean-text)))
               (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
      ;; Send numbered text with [001] format
      (vterm-send-string
       (format "[%03d] %s\n" chunk-id clean-text)))))

(defun whisper-live--insert-chunk-term (chunk-id clean-text)
  "Insert CLEAN-TEXT into term buffer with CHUNK-ID."
  ;; Only proceed if we have meaningful text
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    ;; Send numbered text with [001] format
    (term-send-string (get-buffer-process (current-buffer))
                      (format "[%03d] %s\n" chunk-id clean-text))))

(defun whisper-live--insert-chunk-buffer (chunk-id clean-text)
  "Insert CLEAN-TEXT into regular buffer with CHUNK-ID."
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    (let ((inhibit-read-only t))
      ;; Go to end marker and insert numbered text with [001] format
      (goto-char whisper-live--insert-end-marker)
      (when whisper-live-clean-with-llm
        (insert whisper-live-start-tag))
      (insert (format "[%03d] %s\n" chunk-id clean-text))
      (when whisper-live-clean-with-llm
        (insert whisper-live-end-tag))
      (set-marker whisper-live--insert-end-marker (point)))))

(defun whisper-live--extract-last-words (text &optional num-words)
  "Extract the last NUM-WORDS words from TEXT.
If NUM-WORDS is nil, uses `whisper-live-display-words`.
If NUM-WORDS is nil and `whisper-live-display-words` is nil, returns full text."
  (when (and text (not (string-empty-p text)))
    (let* ((word-limit (or num-words whisper-live-display-words))
           (trimmed (string-trim text)))
      (if word-limit
          ;; Split into words and take last N
          (let* ((words (split-string trimmed))
                 (word-count (length words))
                 (start-idx (max 0 (- word-count word-limit)))
                 (last-words (seq-subseq words start-idx)))
            (string-join last-words " "))
        ;; No limit - return full text
        trimmed))))

(defun whisper-live--check-voice-command (text)
  "Check if TEXT contains a voice command and execute it.
Returns the command function if found, nil otherwise.
Voice commands are only recognized in properly transcribed text.
The text is the raw transcription output before chunk ID is added."
  (when (and whisper-live-voice-commands-enabled
             text
             (not (string-empty-p (string-trim text)))
             ;; Only process if it looks like valid speech (has some letters)
             (string-match-p "[a-zA-Z]" text))
    (let ((lower-text (downcase (string-trim text))))
      (cl-loop for (cmd . func) in whisper-live-voice-commands
               ;; Match command phrase (case-insensitive)
               when (string-match-p (regexp-quote cmd) lower-text)
               do (progn
                    (message "[whisper-live] Voice command detected: '%s' in chunk #%d"
                             cmd (1+ whisper-live--chunk-id))
                    ;; Execute command after a short delay to finish current processing
                    (run-with-timer 0.1 nil func)
                    (cl-return func))))))

(defun whisper-live--handle-transcription (text)
  "Handle transcription TEXT by inserting into buffer.
Stores chunk data and outputs only last N words (controlled by `whisper-live-display-words`).
Also checks for voice commands."
  ;; Check for voice commands first - if detected, skip inserting command text
  (unless (whisper-live--check-voice-command text)
    (when (and text
               (not whisper-live--canceling)  ; Skip if canceling
               (not (string-empty-p (string-trim text)))
               (not (string-match-p "^[[:space:].,]*$" text))
               (markerp whisper-live--insert-marker)
               (markerp whisper-live--insert-end-marker))
    ;; Update activity time for auto-stop tracking
    (whisper-live--update-activity-time)
    (setq whisper-live--transcription-text
          (whisper-live--clean-transcript text))
    (let* ((target-buffer (marker-buffer whisper-live--insert-marker))
           (clean-text
            (whisper--live-remove-tags
             whisper-live--transcription-text))
           ;; Extract only last N words for display
           (display-text (whisper-live--extract-last-words clean-text)))
      (when (and (buffer-live-p target-buffer)
                 (not (string-empty-p clean-text))
                 (not (string-empty-p display-text)))
        ;; Increment chunk ID and store chunk data
        (setq whisper-live--chunk-id (1+ whisper-live--chunk-id))
        (let ((chunk-entry (list :id whisper-live--chunk-id
                                 :raw text
                                 :text clean-text         ;; Store full text
                                 :display display-text    ;; Store displayed portion
                                 :time (current-time))))
          (push chunk-entry whisper-live--chunks)
          ;; Output only LAST N WORDS with chunk-based numbering
          (with-current-buffer target-buffer
            (cond
             ((derived-mode-p 'vterm-mode)
              ;; VTerm: send numbered text
              (whisper-live--insert-chunk-vterm whisper-live--chunk-id display-text))
             ((derived-mode-p 'term-mode)
              ;; Term: send numbered text
              (whisper-live--insert-chunk-term whisper-live--chunk-id display-text))
             (buffer-read-only
              (message "Read-only buffer: [%03d] %s" whisper-live--chunk-id display-text))
             (t
              ;; Regular buffer: insert numbered text
              (whisper-live--insert-chunk-buffer whisper-live--chunk-id display-text))))
          (run-hooks 'whisper-live-transcribe-hook)))))))  ; close unless

(defun whisper-live--transcribe-chunk (concatenated-file)
  "Transcribe a single CONCATENATED-FILE."
  (let ((cmd (whisper-command concatenated-file))
        (temp-buffer (generate-new-buffer " *whisper-temp*"))
        (start-time (current-time)))
    (when (and cmd (car cmd))
      ;; Record start time
      (setq whisper-live--last-transcription-time start-time)
      ;; Kill any existing transcription process first (silently)
      (when (and whisper-live--current-transcription
                 (process-live-p whisper-live--current-transcription))
        (delete-process whisper-live--current-transcription))
      (setq whisper-live--current-transcription
            (make-process
             :name "whisper-live-transcribing"
             :command cmd
             :buffer temp-buffer
             :sentinel (lambda (process event)
                         (let
                             ((process-buffer (process-buffer process)))
                           ;; Update mode line when transcription finishes
                           (force-mode-line-update t)
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
                               ;; Debug: show raw output only when text is empty
                               (when
                                   (or (not text)
                                       (string-empty-p
                                        (string-trim text)))
                                 (message
                                  "[whisper-live] DEBUG - Empty text! Raw output:\n%s"
                                  (substring raw-output 0
                                             (min 1000
                                                  (length raw-output)))))
                               ;; Save full raw output to file if debug enabled
                               (when whisper-live-debug-output
                                 (let ((debug-file
                                        (format "/tmp/whisper-live-debug-%s.txt"
                                                (format-time-string "%Y%m%d-%H%M%S"))))
                                   (with-temp-file debug-file
                                     (insert raw-output))
                                   (message "[whisper-live] Debug output saved to: %s" debug-file)))
                               ;; Show brief info in messages (only if empty or verbose mode)
                               (when (or (not text)
                                        (string-empty-p (string-trim text)))
                                 (message
                                  "[whisper-live] Transcription #%d took %.2fs: %s"
                                  (1+ whisper-live--sentence-counter)
                                  duration
                                  (if text
                                      (substring text 0
                                                 (min 50 (length text)))
                                    "empty")))
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
                         ;; Check volume threshold before processing
                         (if (whisper-live--chunk-has-speech-p chunk-file)
                             (progn
                               ;; Beep without verbose messages
                               (when whisper-live-beep-on-chunk
                                 (whisper-live--beep whisper-live-beep-chunk-frequency 200 2))
                               ;; Choose transcription mode based on setting
                               (let ((file-to-transcribe
                                      (if whisper-live-independent-chunks
                                          ;; Independent mode: transcribe single chunk
                                          chunk-file
                                        ;; Concatenated mode: combine chunks for context
                                        (whisper-live--concatenate-chunks
                                         whisper-live--chunks-directory))))
                                 (push file-to-transcribe
                                       whisper-live--transcription-queue)
                                 (whisper-live--process-transcription-queue)))
                           ;; Volume too low - skip transcription, start next recording
                           (whisper-live--record-chunk))))))))


(provide 'whisper-live-transcribe)

(when
    (not load-file-name)
  (message "whisper-live-transcribe.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))