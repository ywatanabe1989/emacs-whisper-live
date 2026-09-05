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
(require 'whisper-live-accumulative)

;; Defined by whisper-live-llm, which is loaded after this module.
(defvar whisper-live-clean-with-llm nil)
(defvar whisper-live-start-tag nil)
(defvar whisper-live-end-tag nil)

;; Transcription backend configuration

(defvar whisper-live-initial-prompt ""
  "Optional vocabulary prompt passed to the local Whisper backend.")

(defvar whisper-live-remote-host nil
  "SSH host used for transcription, or nil to run Whisper locally.")

(defvar whisper-live-remote-model nil
  "Model name used by the remote transcription backend.
When nil, derive the name from `whisper-model' and `whisper-quantize'.")

(defvar whisper-live-remote-program
  (expand-file-name
   "../scripts/whisper-live-remote"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Local helper program that streams audio to the remote backend over SSH.")

(defvar whisper-live-number-chunks t
  "When non-nil, include a chunk number in inserted transcriptions.")

(defvar whisper-live-chunk-format "[%03d] %s\n"
  "Format string used for numbered transcription chunks.")

(defvar whisper-live-unnumbered-separator " "
  "Text inserted after each transcription when chunk numbers are hidden.")

(defun whisper-live--model-name ()
  "Return the configured Whisper model name, including quantization."
  (if whisper-quantize
      (format "%s-%s" whisper-model whisper-quantize)
    whisper-model))

(defun whisper-live--command (input-file)
  "Build the transcription command for INPUT-FILE.
Use `whisper-live-remote-host' when configured, otherwise use the bundled
local whisper.cpp command builder."
  (if whisper-live-remote-host
      (list whisper-live-remote-program
            "--host" whisper-live-remote-host
            "--language" whisper-language
            "--model" (or whisper-live-remote-model
                          (whisper-live--model-name))
            "--file" input-file)
    (let ((whisper--install-path
           (or whisper--install-path
               (expand-file-name "whisper.cpp/"
                                 whisper-install-directory))))
      (append (whisper-command input-file)
              (when (and whisper-live-initial-prompt
                         (not (string-empty-p whisper-live-initial-prompt)))
                (list "--prompt" whisper-live-initial-prompt))))))

(defun whisper-live--format-chunk (chunk-id text)
  "Format transcription TEXT with CHUNK-ID for insertion."
  (if whisper-live-number-chunks
      (format whisper-live-chunk-format chunk-id text)
    (concat text whisper-live-unnumbered-separator)))

(defun whisper-live--replace-chunk-vterm (chunk-id clean-text)
  "Replace provisional vterm input with CLEAN-TEXT from CHUNK-ID."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (ignore proc)
    (dotimes (_ whisper-live--last-sent-length)
      (vterm-send-backspace))
    (when-let ((prefix (whisper-live--maybe-insert-prefix)))
      (vterm-send-string prefix))
    (let ((replacement (whisper-live--format-chunk chunk-id clean-text)))
      (vterm-send-string replacement)
      (setq whisper-live--last-sent-length (length replacement)))))

(defun whisper-live--replace-chunk-term (chunk-id clean-text)
  "Replace provisional term input with CLEAN-TEXT from CHUNK-ID."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (when (> whisper-live--last-sent-length 0)
      (term-send-string proc
                        (make-string whisper-live--last-sent-length 127)))
    (when-let ((prefix (whisper-live--maybe-insert-prefix)))
      (term-send-string proc prefix))
    (let ((replacement (whisper-live--format-chunk chunk-id clean-text)))
      (term-send-string proc replacement)
      (setq whisper-live--last-sent-length (length replacement)))))

(defun whisper-live--replace-chunk-buffer (chunk-id clean-text)
  "Replace provisional regular-buffer input with CLEAN-TEXT from CHUNK-ID."
  (let ((inhibit-read-only t)
        (replacement (whisper-live--format-chunk chunk-id clean-text)))
    (goto-char whisper-live--insert-marker)
    (delete-region whisper-live--insert-marker whisper-live--insert-end-marker)
    ;; Keep an optional one-time prefix outside the revisable region.
    (when-let ((prefix (whisper-live--maybe-insert-prefix)))
      (insert prefix)
      (set-marker whisper-live--insert-marker (point)))
    (when whisper-live-clean-with-llm
      (insert whisper-live-start-tag))
    (insert replacement)
    (when whisper-live-clean-with-llm
      (insert whisper-live-end-tag))
    (set-marker whisper-live--insert-end-marker (point))))

;; Prefix insertion configuration

(defvar whisper-live-insert-prefix nil
  "When non-nil, insert a prefix message before the first transcription.")

(defvar whisper-live-prefix-message "[Transcription started]\n"
  "Prefix message to insert before first transcription when enabled.")

(defvar whisper-live--prefix-inserted nil
  "Internal flag tracking if prefix has been inserted in current session.")

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
             "whisper_print_progress_callback: progress = +[0-9]+%?"
	     ""
             cleaned))
      ;; Remove any whisper_ system messages that might be inline
      (setq cleaned
            (replace-regexp-in-string
             "whisper_[a-z_]+:" ""
             cleaned))
      ;; Remove ALL bracketed content: [...], (...), {...}, <...>
      (setq cleaned
            (replace-regexp-in-string
             "\\[[^]]*\\]\\|([^)]*)\\|{[^}]*}\\|<[^>]*>" ""
             cleaned))
      ;; Remove line-start whisper messages
      (setq cleaned
            (replace-regexp-in-string
             "^whisper_.*$" ""
             cleaned))
      (string-trim cleaned))))

(defun whisper-live--extract-text-from-output (buffer)
  "Extract transcribed text from whisper output BUFFER.
Whisper outputs transcription to stdout and everything else to stderr.
When using :buffer without :stderr, we only get stdout which is the transcription."
  (with-current-buffer buffer
    (let ((output (string-trim (buffer-string))))
      ;; Debug: show what we got from stdout
      (whisper-live-debug-message "stdout buffer: '%s'" output)
      ;; If we captured only stdout, the output should be just the transcription
      ;; Just filter out any remaining system lines that might have leaked through
      (if (and (> (length output) 0)
               (not (string-match-p "^whisper_" output))
               (not (string-match-p "^main:" output))
               (not (string-match-p "^system_info:" output))
               (not (string-match-p " = [0-9.]+ |" output))
               (not (string-match-p "time\\s-*=" output)))
          (progn
            (whisper-live-debug-message "Extracted: '%s'" output)
            output)
        (whisper-live-debug-message "Filtered out or empty: '%s'"
				    output)
        nil))))

(defun whisper-live--maybe-insert-prefix ()
  "Insert prefix message if enabled and not yet inserted in this session."
  (when (and whisper-live-insert-prefix
             (not whisper-live--prefix-inserted))
    (setq whisper-live--prefix-inserted t)
    whisper-live-prefix-message))

(defun whisper-live--insert-chunk-vterm (chunk-id clean-text)
  "Insert CLEAN-TEXT into vterm buffer with CHUNK-ID."
  (when-let ((proc (get-buffer-process (current-buffer))))
    ;; Only proceed if we have meaningful text
    (when (and (not (string-empty-p clean-text))
               (not (string-empty-p (string-trim clean-text)))
               (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
      ;; Insert prefix if this is the first transcription
      (when-let ((prefix (whisper-live--maybe-insert-prefix)))
        (vterm-send-string prefix))
      (vterm-send-string
       (whisper-live--format-chunk chunk-id clean-text)))))

(defun whisper-live--insert-chunk-term (chunk-id clean-text)
  "Insert CLEAN-TEXT into term buffer with CHUNK-ID."
  ;; Only proceed if we have meaningful text
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    ;; Insert prefix if this is the first transcription
    (when-let ((prefix (whisper-live--maybe-insert-prefix)))
      (term-send-string (get-buffer-process (current-buffer)) prefix))
    (term-send-string (get-buffer-process (current-buffer))
                      (whisper-live--format-chunk chunk-id clean-text))))

(defun whisper-live--insert-chunk-buffer (chunk-id clean-text)
  "Insert CLEAN-TEXT into regular buffer with CHUNK-ID."
  (when (and (not (string-empty-p clean-text))
             (not (string-empty-p (string-trim clean-text)))
             (not (string-match-p "^[[:space:].,!?;:]*$" clean-text)))
    (let ((inhibit-read-only t)
          (prefix (whisper-live--maybe-insert-prefix)))
      ;; Go to the end marker and insert the formatted transcription.
      (goto-char whisper-live--insert-end-marker)
      ;; Insert prefix if this is the first transcription
      (when prefix
        (insert prefix))
      (when whisper-live-clean-with-llm
        (insert whisper-live-start-tag))
      (insert (whisper-live--format-chunk chunk-id clean-text))
      (when whisper-live-clean-with-llm
        (insert whisper-live-end-tag))
      (set-marker whisper-live--insert-end-marker (point)))))

(defun whisper-live--extract-last-words (text &optional num-words)
  "Extract the last NUM-WORDS words from TEXT.
If NUM-WORDS is nil, uses `whisper-live-display-words`.
If NUM-WORDS is nil and `whisper-live-display-words` is nil, returns full text."
  (when-let ((trimmed (and text (string-trim text))))
    (unless (string-empty-p trimmed)
      (let ((word-limit (or num-words whisper-live-display-words)))
      (if word-limit
          ;; Split into words and take last N
          (let* ((words (split-string trimmed))
                 (word-count (length words))
                 (start-idx (max 0 (- word-count word-limit)))
                 (last-words (seq-subseq words start-idx)))
            (string-join last-words " "))
        ;; No limit - return full text
          trimmed)))))

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
                    (whisper-live-debug-message
		     "Voice command detected: '%s' in chunk #%d"
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
      (let*
	  ((target-buffer (marker-buffer whisper-live--insert-marker))
           (mode (whisper-live--get-transcription-mode))
           (revise-p (and (eq mode 'accumulative)
                          whisper-live-accumulative-revise-text))
           (clean-text
            (whisper--live-remove-tags
             whisper-live--transcription-text))
           ;; Extract text for display based on mode
           (display-text
            (cond
             (revise-p
              ;; Reinsert the complete, increasingly contextual transcript.
              clean-text)
             ((eq mode 'accumulative)
                ;; Accumulative mode: use text diffing
              (whisper-live--accumulative-diff-text clean-text))
             (t
              ;; Other modes: extract last N words
              (whisper-live--extract-last-words clean-text)))))
	(when (and (buffer-live-p target-buffer)
                   (not (string-empty-p clean-text))
                   (not (string-empty-p display-text)))
          (when revise-p
            (setq whisper-live--previous-transcription clean-text))
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
		(if revise-p
                    (whisper-live--replace-chunk-vterm
                     whisper-live--chunk-id display-text)
                  (whisper-live--insert-chunk-vterm
                   whisper-live--chunk-id display-text)))
               ((derived-mode-p 'term-mode)
		(if revise-p
                    (whisper-live--replace-chunk-term
                     whisper-live--chunk-id display-text)
                  (whisper-live--insert-chunk-term
                   whisper-live--chunk-id display-text)))
               (buffer-read-only
		(message "Read-only buffer: [%03d] %s"
			 whisper-live--chunk-id display-text))
               (t
		(if revise-p
                    (whisper-live--replace-chunk-buffer
                     whisper-live--chunk-id display-text)
                  (whisper-live--insert-chunk-buffer
                   whisper-live--chunk-id display-text))))
            (run-hooks 'whisper-live-transcribe-hook))))))))
					; close unless
(defun whisper-live--transcribe-chunk (concatenated-file)
  "Transcribe a single CONCATENATED-FILE."
  (let ((cmd (whisper-live--command concatenated-file))
        (temp-buffer (generate-new-buffer " *whisper-temp*"))
        (start-time (current-time)))
    (when (and cmd (car cmd))
      ;; Record start time
      (setq whisper-live--last-transcription-time start-time)
      ;; Kill any existing transcription process first (silently)
      (when (and whisper-live--current-transcription
                 (process-live-p whisper-live--current-transcription))
        (delete-process whisper-live--current-transcription))
      ;; Create/get debug buffer for raw output
      (let ((debug-buffer (get-buffer-create "*whisper-live-debug*")))
        (setq whisper-live--current-transcription
              (make-process
               :name "whisper-live-transcribing"
               :command cmd
               :buffer temp-buffer
               :stderr debug-buffer  ; Capture stderr to debug buffer
               :sentinel (lambda (process event)
                           (let
                               ((process-buffer
				 (process-buffer process)))
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
                                     (with-current-buffer
					 process-buffer
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
                                          (format
					   "/tmp/whisper-live-debug-%s.txt"
                                           (format-time-string
					    "%Y%m%d-%H%M%S"))))
                                     (with-temp-file debug-file
                                       (insert raw-output))
                                     (message
				      "[whisper-live] Debug output saved to: %s"
				      debug-file)))
				 ;; Show brief info in messages (only if empty or verbose mode)
				 (when (or (not text)
                                           (string-empty-p
					    (string-trim text)))
                                   (message
                                    "[whisper-live] Transcription #%d took %.2fs: %s"
                                    (1+ whisper-live--sentence-counter)
                                    duration
                                    (if text
					(substring text 0
                                                   (min 50
							(length text)))
                                      "empty")))
				 (whisper-live--handle-transcription
                                  text))
                               (setq
				whisper-live--current-transcription
                                nil)
                               (whisper-live--process-transcription-queue)
                               (when (and whisper-live--stop-after-transcription
                                          (not whisper-live--current-transcription)
                                          (not whisper-live--transcription-queue)
                                          (not (process-live-p
                                                whisper-live--current-process))
                                          (fboundp 'whisper-live-stop))
                                 (whisper-live-stop))
                               (kill-buffer process-buffer))))))))))
					; extra paren for debug-buffer let
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
                         (if
			     (whisper-live--chunk-has-speech-p
			      chunk-file)
                             (let
                                 ((file-to-transcribe
                                   (whisper-live--accumulative-get-file-for-transcription
                                    chunk-file)))
                               ;; Beep without verbose messages
                               (when whisper-live-beep-on-chunk
                                 (whisper-live--beep
				  whisper-live-beep-chunk-frequency
				  200 2))
                               (push file-to-transcribe
                                     whisper-live--transcription-queue)
                               ;; Recording and GPU inference now overlap.  At
                               ;; the configured context limit, wait for this
                               ;; final full transcription and stop cleanly.
                               (when (and
                                      (eq (whisper-live--get-transcription-mode)
                                          'accumulative)
                                      whisper-live-accumulative-stop-at-max-duration
                                      (>= whisper-live--accumulated-duration
                                          whisper-live-accumulative-max-duration))
                                 (setq whisper-live--stop-after-transcription t))
                               (unless whisper-live--stop-after-transcription
                                 (whisper-live--record-chunk))
                               (whisper-live--process-transcription-queue))
                           ;; A quiet window needs no inference; continue at once.
                           (if whisper-live--stop-after-transcription
                               (when (and
                                      (not whisper-live--current-transcription)
                                      (not whisper-live--transcription-queue)
                                      (fboundp 'whisper-live-stop))
                                 (whisper-live-stop))
                             (whisper-live--record-chunk)))))))))

(provide 'whisper-live-transcribe)

(when
    (not load-file-name)
  (message "whisper-live-transcribe.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
