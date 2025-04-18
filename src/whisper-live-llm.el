;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-08 18:06:47
;;; Time-stamp: <2024-12-08 19:17:19 (ywatanabe)>
;;; File: ./whisper-live/src/whisper-live-llm.el


(require 'request)
(require 'whisper-live-core)

(defcustom whisper-live-anthropic-key (or (getenv "ANTHROPIC_API_KEY") "")
  "API key for Anthropic Claude. Defaults to ANTHROPIC_API_KEY environment variable."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model engine for Anthropic Claude."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-clean-with-llm nil
  "Whether to clean transcriptions using LLM (AI language model)."
  :type 'boolean
  :group 'whisper-live
  :safe #'booleanp)

(defvar whisper-live-llm-prompt
  "Clean up the following raw text transcribed from audio. Fix minor errors to produce natural language output. As long as meaning is remained, you can revise as a English native speaker. Respond with only the corrected text and NEVER INCLUDE YOUR COMMENTS. Now, the raw transcription is as follows: \n"
  "Prompt text used for LLM-based transcription cleanup.")

(defcustom whisper-live-start-tag-base "Whisper"
  "Tag to prepend at start of transcription."
  :type 'string
  :group 'whisper-live)

(defcustom whisper-live-end-tag-base "Whisper"
  "Tag to append at end of transcription."
  :type 'string
  :group 'whisper-live)

(defvar whisper-live-start-tag nil
  "Tag to prepend at start of transcription.")

(defvar whisper-live-end-tag nil
  "Tag to append at end of transcription.")

(defun whisper-live--get-start-tag ()
  "Get start tag based on LLM setting."
  (concat (if whisper-live-clean-with-llm
              (concat whisper-live-start-tag-base " + LLM")
            whisper-live-start-tag-base)
          " => "))

(defun whisper-live--get-end-tag ()
  "Get end tag based on LLM setting."
  (concat " <= " (if whisper-live-clean-with-llm
                     (concat whisper-live-end-tag-base " + LLM")
                   whisper-live-end-tag-base)))

(defun whisper-live--update-tags ()
  "Update tags based on current LLM setting."
  (setq whisper-live-start-tag (whisper-live--get-start-tag)
        whisper-live-end-tag (whisper-live--get-end-tag)))

(add-variable-watcher 'whisper-live-clean-with-llm
                      (lambda (sym newval op where)
                        (whisper-live--update-tags)))

(defun whisper--live-find-tags ()
  "Find start and end positions of transcription tags."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (regexp-quote whisper-live-start-tag) nil t)
      (let ((start-pos (match-beginning 0)))
        (when (re-search-forward (regexp-quote whisper-live-end-tag) nil t)
          (cons start-pos (match-end 0)))))))

(defun whisper--live-remove-tags (text)
  "Remove whisper live tags from TEXT."
  (when text
    (let ((clean-text (replace-regexp-in-string
                       (format "%s\\|%s"
                               (regexp-quote whisper-live-start-tag)
                               (regexp-quote whisper-live-end-tag))
                       "" text)))
      (if whisper-live-clean-with-llm
          (format "%s%s%s" whisper-live-start-tag clean-text whisper-live-end-tag)
        clean-text))))

(defun whisper-live--clean-raw-transcription-with-llm (raw-transcription)
  "Clean RAW-TRANSCRIPTION using LLM."
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

(defun whisper-live--llm-post-transcribe-hook ()
  "Hook run after transcription to clean with LLM if enabled."
  (when (and whisper-live-clean-with-llm
             whisper-live--transcription-text)
    (let ((cleaned-text (whisper-live--clean-raw-transcription-with-llm
                         whisper-live--transcription-text)))
      (when cleaned-text
        (with-current-buffer (marker-buffer whisper-live--insert-marker)
          (save-excursion
            (goto-char whisper-live--insert-marker)
            (delete-region whisper-live--insert-marker whisper-live--insert-end-marker)
            (insert cleaned-text)
            (set-marker whisper-live--insert-end-marker (point))))))))

(add-hook 'whisper-live-transcribe-hook #'whisper-live--llm-post-transcribe-hook)

(provide 'whisper-live-llm)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
