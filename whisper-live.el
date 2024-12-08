;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 16:42:18
;;; Time-stamp: <2024-12-08 17:22:49 (ywatanabe)>
;;; File: ./whisper-live/whisper-live.el


;;; whisper-live.el --- Real-time speech transcription using Whisper -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Yusuke Watanabe

;; Author: Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac.jp>
;; URL: https://github.com/ywatanabe1989/whisper-live.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (whisper "0.1.0"))
;; Keywords: multimedia, speech recognition, transcription

;;; Commentary:
;;
;; whisper-live.el provides real-time speech transcription using Whisper.
;; It records audio in chunks and transcribes them on the fly.
;;
;; Features:
;; - Real-time audio transcription
;; - LLM-based cleanup of transcriptions (optional)
;; - Customizable chunk duration and buffer settings
;;
;; Usage:
;; M-x whisper-live-run   ; Toggle transcription

;;; Code:

(require 'request)
(require 'whisper)
(require 'cl-lib)

(defvar whisper--temp-file (make-temp-file (whisper-live--generate-chunk-filename)))

(defcustom whisper-live--max-history-seconds 15
  "Maximum duration of audio history to keep in seconds.")

(defcustom whisper-live-chunk-duration 5
  "Duration of each audio chunk in seconds."
  :type 'integer
  :group 'whisper)

(defcustom whisper-live-buffer-name "*Whisper Live*"
  "Name of the buffer where real-time transcription appears."
  :type 'string
  :group 'whisper)

(defvar whisper-live--initialized nil
  "Flag indicating whether whisper has been initialized.")

(defvar whisper-live--current-process nil
  "Current recording process.")

(defun whisper-live--generate-chunks-directory ()
  "Directory to store temporary audio chunks."
  (message "new directory generated")
  (setq whisper-live--chunks-directory
        (concat temporary-file-directory
                (format-time-string "whisper-live-chunks/%Y%m%d-%H%M%S/"))))

(defun whisper-live--update-chunks-directory ()
  "Update the chunks directory with a new timestamp."
  (setq whisper-live--chunks-directory (whisper-live--generate-chunks-directory))
  (make-directory whisper-live--chunks-directory t))


(defvar whisper-live--chunks-directory
  (whisper-live--update-chunks-directory)
  "Directory to store temporary audio chunks.")

(defvar whisper-live--text-queue '()
  "Queue of recent transcriptions.")

(defcustom whisper-live-max-history 10
  "Maximum number of transcription chunks to keep in history."
  :type 'integer
  :group 'whisper)

(defcustom whisper-live--max-chunks 30
  "Maximum number of transcription chunks to keep in history."
  :type 'integer
  :group 'whisper)

(defvar whisper-live--target-buffer nil
  "Buffer where transcribed text should be inserted.")

(defvar whisper-live--insert-marker nil
  "Marker for insertion position.")

(defvar whisper-live--insert-end-marker nil
  "Marker for end of insertion position.")

(defcustom whisper-live-anthropic-key (or (getenv "ANTHROPIC_API_KEY") "")
  "API key for Anthropic Claude. Defaults to ANTHROPIC_API_KEY environment variable."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model engine for Anthropic Claude."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-clean-with-llm nil
  "Whether to clean transcriptions using LLM (AI language model).
When enabled, transcriptions will be post-processed by an LLM to improve accuracy."
  :type 'boolean
  :group 'whisper-live
  :safe #'booleanp)

(defvar whisper-live-llm-prompt
  "Clean up the following raw text transcribed from audio. Fix minor errors to produce natural language output. As long as meaning is remained, you can revise as a English native speaker. Respond with only the corrected text and NEVER INCLUDE YOUR COMMENTS. Now, the raw transcription is as follows: \n"
  "Prompt text used for LLM-based transcription cleanup.")

;; Tag
(defcustom whisper-live-start-tag-base "Whisper"
  "Tag to prepend at start of transcription."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-end-tag-base "Whisper"
  "Tag to append at end of transcription."
  :type 'string
  :group 'whisper-live)

(defun whisper-live--get-start-tag ()
  "Get start tag based on LLM setting."
  (let ((tag (concat (if whisper-live-clean-with-llm
                         (concat whisper-live-start-tag-base " + LLM")
                       whisper-live-start-tag-base)
                     " => ")))
    ;; (message "Generated start tag: %s (LLM: %s)" tag whisper-live-clean-with-llm)
    tag))

(defun whisper-live--get-end-tag ()
  "Get end tag based on LLM setting."
  (let ((tag (concat " <= " (if whisper-live-clean-with-llm
                                (concat whisper-live-end-tag-base " + LLM")
                              whisper-live-end-tag-base))))
    tag))

(defvar whisper-live-start-tag (whisper-live--get-start-tag)
  "Tag to prepend at start of transcription.")

(defvar whisper-live-end-tag (whisper-live--get-end-tag)
  "Tag to append at end of transcription.")

(defun whisper-live--update-tags ()
  "Update tags based on current LLM setting."
  (setq whisper-live-start-tag (whisper-live--get-start-tag)
        whisper-live-end-tag (whisper-live--get-end-tag))
  )

(add-variable-watcher 'whisper-live-clean-with-llm
                      (lambda (sym newval op where)
                        (whisper-live--update-tags)))

(whisper-live--update-tags)



(defun whisper-live--ensure-directory ()
  "Ensure chunks directory exists."
  (unless (file-exists-p whisper-live--chunks-directory)
    (make-directory whisper-live--chunks-directory t)))

(defun whisper-live--generate-chunk-filename ()
  "Generate unique filename for audio chunk."
  (format "%swhisper-chunk-%s.wav"
          whisper-live--chunks-directory
          (format-time-string "%Y%m%d-%H%M%S")))

(defvar whisper-live--output-buffer nil
  "Buffer for storing whisper transcription output.")

(defun whisper-live--ensure-output-buffer ()
  "Create or get the whisper output buffer."
  (unless (buffer-live-p whisper-live--output-buffer)
    (setq whisper-live--output-buffer
          (get-buffer-create "*Whisper Live Output*"))
    (with-current-buffer whisper-live--output-buffer
      (special-mode)
      (buffer-disable-undo)))
  (display-buffer whisper-live--output-buffer)
  whisper-live--output-buffer)


(defvar whisper-live--transcription-queue nil
  "Queue of files waiting to be transcribed.")

(defun whisper-live--process-transcription-queue ()
  "Process the next item in the transcription queue."
  (when (and whisper-live--transcription-queue
             (not whisper-live--current-transcription))
    (let ((next-file (pop whisper-live--transcription-queue)))
      (setq whisper-live--current-transcription t)
      (whisper-live--transcribe-chunk next-file))))

(defun whisper-live--add-to-queue (file)
  "Add FILE to transcription queue and process if possible."
  (push file whisper-live--transcription-queue)
  (whisper-live--process-transcription-queue))

;; (defun whisper-live--transcribe-chunk (concatenated-file)
;;   "Transcribe a single CONCATENATED-FILE."
;;   (message "Processing transcription: %s" concatenated-file)
;;   (let ((cmd (whisper-command concatenated-file))
;;         (temp-buffer (generate-new-buffer " *whisper-temp*")))
;;     (when (and cmd (car cmd))
;;       (make-process
;;        :name "whisper-live-transcribing"
;;        :command cmd
;;        :buffer temp-buffer
;;        :sentinel (lambda (_process event)
;;                    (when (string-equal "finished\n" event)
;;                      (let ((text (with-current-buffer temp-buffer
;;                                    (when (string-match "\n\n \\(.*\\)\n\n" (buffer-string))
;;                                      (match-string 1 (buffer-string))))))
;;                        (when text
;;                          (with-current-buffer (marker-buffer whisper-live--insert-marker)
;;                            (save-excursion
;;                              ;; Clear previous text
;;                              (goto-char (point-min))
;;                              (when (search-forward whisper-live-start-tag nil t)
;;                                (delete-region (point) (point-max)))
;;                              ;; Insert new text
;;                              (insert (whisper-live--clean-transcript text) " ")))))
;;                      (setq whisper-live--current-transcription nil)
;;                      (whisper-live--process-transcription-queue)
;;                      (kill-buffer temp-buffer)))))))


(defun whisper-live--transcribe-chunk (concatenated-file)
  "Transcribe a single CONCATENATED-FILE."
  (message "Processing transcription: %s" concatenated-file)
  (let ((cmd (whisper-command concatenated-file))
        (temp-buffer (generate-new-buffer " *whisper-temp*")))
    (when (and cmd (car cmd))
      (make-process
       :name "whisper-live-transcribing"
       :command cmd
       :buffer temp-buffer
       :sentinel (lambda (_process event)
                   (when (string-equal "finished\n" event)
                     (let ((text (with-current-buffer temp-buffer
                                   (when (string-match "\n\n \\(.*\\)\n\n" (buffer-string))
                                     (match-string 1 (buffer-string))))))
                       (when (and text
                                  (not (string-empty-p (string-trim text)))
                                  (not (string-match-p "^[[:space:].,]*$" text)))
                         (with-current-buffer (marker-buffer whisper-live--insert-marker)
                           (save-excursion
                             (goto-char (point-min))
                             (when (search-forward whisper-live-start-tag nil t)
                               (delete-region (point) (point-max)))
                             (insert (whisper-live--clean-transcript text) " ")))))
                     (setq whisper-live--current-transcription nil)
                     (whisper-live--process-transcription-queue)
                     (kill-buffer temp-buffer)))))))

(defun whisper-live--concatenate-chunks (chunk-directory)
  "Concatenate recent wav chunks in CHUNK-DIRECTORY into single file."
  (let* ((output-file (concat chunk-directory "combined.wav"))
         (chunk-files (directory-files chunk-directory t "whisper-chunk-.*\\.wav$"))
         (sorted-chunks (sort chunk-files #'string<))
         (recent-chunks (last sorted-chunks whisper-live--max-chunks))
         (chunks-list (concat chunk-directory "chunks.txt")))
    (when recent-chunks
      (with-temp-file chunks-list
        (dolist (chunk recent-chunks)
          (insert (format "file '%s'\n" chunk))))
      (call-process "ffmpeg" nil nil nil
                    "-f" "concat"
                    "-safe" "0"
                    "-i" chunks-list
                    "-c" "copy"
                    "-y"
                    output-file)
      ;; Delete old chunks
      (let ((old-chunks (butlast sorted-chunks whisper-live--max-chunks)))
        (dolist (chunk old-chunks)
          (when (file-exists-p chunk)
            (delete-file chunk))))
      output-file)))

(defun whisper-live--clean-transcript (text)
  "Clean transcript TEXT by removing parenthetical and bracket expressions."
  (when text
    (let ((cleaned (replace-regexp-in-string "\\[.*?\\]\\|([^)]*)" "" text)))
      (string-trim cleaned))))

(defvar whisper-live--transcription-queue '()
  "Queue of files waiting to be transcribed.")

(defvar whisper-live--current-transcription nil
  "Currently running transcription process.")

(defun whisper-live--process-transcription-queue ()
  "Process next file in transcription queue if available."
  (message "Processing queue: %d items" (length whisper-live--transcription-queue))
  (when (and (not whisper-live--current-transcription)
             whisper-live--transcription-queue)
    (let ((next-file (pop whisper-live--transcription-queue)))
      (message "Transcribing file: %s" next-file)
      (setq whisper-live--current-transcription
            (whisper-live--transcribe-chunk next-file)))))

(defun whisper-live--record-chunk ()
  "Record a single audio chunk."
  (let ((chunk-file (whisper-live--generate-chunk-filename)))
    (message "Recording to: %s" chunk-file)
    (setq whisper-live--current-process
          (make-process
           :name "whisper-live-recording"
           :command `("ffmpeg"
                      "-f" ,whisper--ffmpeg-input-format
                      "-i" ,whisper--ffmpeg-input-device
                      "-t" ,(number-to-string whisper-live-chunk-duration)
                      "-ar" "16000"
                      "-y" ,chunk-file)
           :sentinel (lambda (_process event)
                       (when (string-equal "finished\n" event)
                         (message "Recording finished, combining chunks")
                         (let ((combined-file (whisper-live--concatenate-chunks whisper-live--chunks-directory)))
                           (push combined-file whisper-live--transcription-queue)
                           (whisper-live--process-transcription-queue))
                         (whisper-live--record-chunk)))))))

(defun whisper-live-start ()
  "Start real-time transcription."
  (interactive)
  (setq whisper-live--transcription-queue nil
        whisper-live--current-transcription nil)
  (insert whisper-live-start-tag)
  (setq whisper-live--target-buffer (current-buffer)
        whisper-live--insert-marker (point-marker))
  (whisper-live--ensure-directory)
  (whisper-live--record-chunk)
  (message "Live transcribing..."))

(defun whisper-live-stop ()
  "Stop real-time transcription and optionally clean final text with LLM."
  (interactive)
  (when whisper-live--current-process
    (delete-process whisper-live--current-process)
    (setq whisper-live--current-process nil))
  (setq whisper-live--transcription-queue nil
        whisper-live--current-transcription nil)
  (when whisper-live--insert-marker
    (let ((buffer (marker-buffer whisper-live--insert-marker)))
      (when buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char whisper-live--insert-marker)
            (insert whisper-live-end-tag))
          (whisper-live--clean-transcript-after-stop)))))
  (when whisper-live--insert-marker
    (set-marker whisper-live--insert-marker nil))
  (when whisper-live--insert-end-marker
    (set-marker whisper-live--insert-end-marker nil))
  (when (file-exists-p whisper-live--chunks-directory)
    (delete-directory whisper-live--chunks-directory t))
  (message "Stopped."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun whisper--live-find-tags ()
  "Find start and end positions of transcription tags."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (regexp-quote whisper-live-start-tag) nil t)
      (let ((start-pos (match-beginning 0)))
        (when (re-search-forward (regexp-quote whisper-live-end-tag) nil t)
          (cons start-pos (match-end 0)))))))

(defun whisper-live-extract--raw-transcription ()
  "Extract text between transcription tags."
  (interactive)
  (when-let ((tags (whisper--live-find-tags)))
    ;; (message "Found tags: %S" tags)
    (let* ((text (buffer-substring-no-properties (car tags) (cdr tags)))
           (cleaned-text (whisper--live-remove-tags text)))
      ;; (message "Extracted text: '%s'" cleaned-text)
      cleaned-text)))
;; (whisper-live-extract--raw-transcription)

(defun whisper--live-remove-tags (text)
  "Remove whisper live tags from TEXT."
  (interactive)
  (when text
    (replace-regexp-in-string (format "%s\\|%s"
                                      (regexp-quote whisper-live-start-tag)
                                      (regexp-quote whisper-live-end-tag))
                              "" text)))

(defun whisper-live--clean-raw-transcription-with-llm (raw-transcription)
  (when (and whisper-live-anthropic-key
             (not (string-empty-p whisper-live-anthropic-key)))
    (if (string-empty-p raw-transcription)
        raw-transcription
      (condition-case err
          (let* ((full-prompt (concat whisper-live-llm-prompt raw-transcription))
                 (response (request
                             "https://api.anthropic.com/v1/messages"
                             :type "POST"
                             :headers `(("content-type" . "application/json")
                                        ("x-api-key" . ,whisper-live-anthropic-key)
                                        ("anthropic-version" . "2023-06-01"))
                             :data (json-encode
                                    `(("model" . ,whisper-live-anthropic-engine)
                                      ("max_tokens" . 2048)
                                      ("messages" . [,(list (cons "role" "user")
                                                            (cons "content" full-prompt))])))
                             :parser 'json-read
                             :sync t
                             :silent t))
                 (resp-data (request-response-data response)))
            (when resp-data
              (alist-get 'text (aref (alist-get 'content resp-data) 0))))
        (error
         raw-transcription)))))

(defun whisper-live-overwrite--raw-transcription (new-text)
  "Replace text between transcription tags with NEW-TEXT."
  (when-let ((tags (whisper--live-find-tags)))
    (delete-region (car tags) (cdr tags))
    (goto-char (car tags))
    (insert new-text)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun whisper-live-run ()
  "Toggle real-time audio transcription.
If transcription is running, stops it.
If not running, starts new transcription session."
  (interactive)
  (whisper-live--init)
  (whisper-live--update-tags)
  (setq whisper-live--insert-marker (make-marker)
        whisper-live--insert-end-marker (make-marker))
  (if whisper-live--current-process
      (whisper-live-stop)
    (if whisper-install-whispercpp
        (progn
          (whisper--check-model-consistency)
          (whisper-live-start))
      (let ((command (car (whisper-command whisper--temp-file))))
        (if (or (file-exists-p command)
                (executable-find command))
            (whisper-live-start)
          (error (format "Couldn't find %s in PATH, nor is it a file" command)))))))

(defun whisper-live--clean-transcript-after-stop ()
  "Clean transcript after stopping recording."
  (let ((raw-text (whisper-live-extract--raw-transcription)))
    (when raw-text
      (let ((final-text (if whisper-live-clean-with-llm
                            (whisper-live--clean-raw-transcription-with-llm raw-text)
                          raw-text)))
        (when final-text
          (whisper-live-overwrite--raw-transcription final-text))))))

(defun whisper-live--cleanup-temp-files ()
  "Clean up temporary files."
  (when (file-exists-p whisper--temp-file)
    (delete-file whisper--temp-file)))

(add-hook 'kill-emacs-hook #'whisper-live--cleanup-temp-files)

(defun whisper-live--cleanup ()
  "Clean up all whisper-live resources."
  (interactive)
  (when whisper-live--current-process
    (ignore-errors (delete-process whisper-live--current-process)))
  (setq whisper-live--current-process nil)
  (when whisper-live--insert-marker
    (set-marker whisper-live--insert-marker nil))
  (when whisper-live--insert-end-marker
    (set-marker whisper-live--insert-end-marker nil))
  (setq whisper-live--insert-end-marker nil)
  (setq whisper-live--text-queue nil
        whisper-live--target-buffer nil)
  (when (file-exists-p whisper-live--chunks-directory)
    (ignore-errors (delete-directory whisper-live--chunks-directory t)))
  )

(defun whisper-live--init ()
  "Initialize whisper by running it twice to ensure stable startup."
  (whisper-live--generate-chunks-directory)
  (when (not whisper-live--initialized)
    (let ((whisper-buffer-name "*Whisper-Live-Init*")
          (inhibit-message t)
          (message-log-max nil))
      (with-current-buffer (get-buffer-create whisper-buffer-name)
        (advice-add 'whisper--error :override #'ignore)
        (whisper-run)
        (sleep-for 1)
        (whisper-run)
        (advice-remove 'whisper--error #'ignore)
        (kill-buffer whisper-buffer-name)))
    ;; (setq whisper-live--initialized t)
    ))

;; Add to keyboard quit hook
(add-hook 'keyboard-quit-hook #'whisper-live--cleanup)

(provide 'whisper-live)

;;; whisper-live.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
