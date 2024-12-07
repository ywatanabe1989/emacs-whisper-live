;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 11:42:37
;;; Time-stamp: <2024-12-07 11:42:37 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/whisper-live/whisper-live.el


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

(defcustom whisper-live-chunk-duration 5
  "Duration of each audio chunk in seconds."
  :type 'integer
  :group 'whisper)

(defcustom whisper-live-buffer-name "*Whisper Live*"
  "Name of the buffer where real-time transcription appears."
  :type 'string
  :group 'whisper)

(defvar whisper-live--current-process nil
  "Current recording process.")

(defvar whisper-live--chunks-directory
  (concat temporary-file-directory "whisper-live-chunks/")
  "Directory to store temporary audio chunks.")

(defvar whisper-live--text-queue '()
  "Queue of recent transcriptions.")

(defcustom whisper-live-max-history 10
  "Maximum number of transcription chunks to keep in history."
  :type 'integer
  :group 'whisper)

(defvar whisper-live--target-buffer nil
  "Buffer where transcribed text should be inserted.")

(defvar whisper-live--insert-marker nil
  "Marker for insertion position.")

(defcustom whisper-live-anthropic-key (or (getenv "ANTHROPIC_API_KEY") "")
  "API key for Anthropic Claude. Defaults to ANTHROPIC_API_KEY environment variable."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-anthropic-engine "claude-3-5-haiku-20241022"
  "Model engine for Anthropic Claude."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-clean-with-llm t
  "Whether to clean transcriptions using LLM."
  :type 'boolean
  :group 'whisper-live)

(defcustom whisper-live-start-tag "Whisper Transcription ==> "
  "Tag to prepend at start of transcription."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-end-tag " <== Whisper Transcription"
  "Tag to append at end of transcription."
  :type 'string
  :group 'whisper-live)

(defun whisper-live--ensure-directory ()
  "Ensure chunks directory exists."
  (unless (file-exists-p whisper-live--chunks-directory)
    (make-directory whisper-live--chunks-directory t)))

(defun whisper-live--generate-chunk-filename ()
  "Generate unique filename for audio chunk."
  (format "%swhisper-chunk-%s.wav"
          whisper-live--chunks-directory
          (format-time-string "%Y%m%d-%H%M%S")))

(defun whisper-live--transcribe-chunk (chunk-file)
  "Transcribe a single CHUNK-FILE."
  (make-process
   :name "whisper-live-transcribing"
   :command (whisper-command chunk-file)
   :buffer (get-buffer-create "*Whisper Live*")
   :sentinel (lambda (_process event)
               (when (string-equal "finished\n" event)
                 (with-current-buffer (get-buffer "*Whisper Live*")
                   (goto-char (point-min))
                   (when (re-search-forward "\n\n \\(.*\\)\n\n" nil t)
                     (let ((text (match-string 1)))
                       (when (and text (marker-buffer whisper-live--insert-marker))
                         (with-current-buffer (marker-buffer whisper-live--insert-marker)
                           (save-excursion
                             (goto-char whisper-live--insert-marker)
                             (insert (whisper-live--clean-transcript text) " ")
                             (set-marker whisper-live--insert-marker (point))))))))
                 (kill-buffer "*Whisper Live*")
                 (delete-file chunk-file)))))

(defun whisper-live--clean-transcript (text)
  "Clean transcript TEXT by removing parenthetical and bracket expressions."
  (when text
    (let ((cleaned (replace-regexp-in-string "\\[.*?\\]\\|([^)]*)" "" text)))
      (string-trim cleaned))))

(defun whisper-live--record-chunk ()
  "Record a single audio chunk."
  (let ((chunk-file (whisper-live--generate-chunk-filename)))
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
                        (whisper-live--transcribe-chunk chunk-file)
                        (whisper-live--record-chunk)))))))

(defun whisper-live-start ()
  "Start real-time transcription."
  (interactive)
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
  (when (file-exists-p whisper-live--chunks-directory)
    (delete-directory whisper-live--chunks-directory t))
  (message "[-] Stopped."))

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
    (message "Found tags: %S" tags)
    (let* ((text (buffer-substring-no-properties (car tags) (cdr tags)))
           (cleaned-text (whisper--live-remove-tags text)))
      (message "Extracted text: '%s'" cleaned-text)
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
  (condition-case err
      (let* ((full-prompt (concat "Clean this raw transcript. If you do not find suitable answer, please just return empty string: " raw-transcription))
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
     raw-transcription)))

(defun whisper-live-overwrite--raw-transcription (new-text)
  "Replace text between transcription tags with NEW-TEXT."
  (when-let ((tags (whisper--live-find-tags)))
    (delete-region (car tags) (cdr tags))
    (goto-char (car tags))
    (insert new-text)))

(defun whisper-live--clean-transcript-after-stop ()
  "Clean transcript after stopping recording."
  (let* ((raw-text (whisper-live-extract--raw-transcription)))
    (when raw-text
      (run-with-timer 1 nil
                      (lambda ()
                        (let ((final-text (whisper-live--clean-raw-transcription-with-llm raw-text)))
                        ;; (let ((final-text "THIS WILL BE THE OUTPUT OF LLM"))
                          (when final-text
                            (whisper-live-overwrite--raw-transcription final-text))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whisper-live-clear-buffer ()
  "Clear the transcription buffer."
  (interactive)
  (with-current-buffer (get-buffer-create whisper-live-buffer-name)
    (erase-buffer)))


;;;###autoload
(defun whisper-live-run ()
  "Toggle real-time audio transcription.
If transcription is running, stops it.
If not running, starts new transcription session."
  (interactive)
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

;; Add keyboard quit hook
(add-hook 'keyboard-quit-hook #'whisper-live-stop)

(provide 'whisper-live)

;;; whisper-live.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
