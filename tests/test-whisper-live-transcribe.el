;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/whisper-live/tests/test-whisper-live-transcribe.el

;;; Commentary:
;; Transcription tests for whisper-live

;;; Code:

(require 'ert)

;; Add parent directory to load path
(let ((default-directory (file-name-directory
                          (directory-file-name
                           (file-name-directory load-file-name)))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (expand-file-name "src" default-directory)))

(require 'whisper-live-core)
(require 'whisper-live-transcribe)

;;; Clean Transcript Tests

(ert-deftest test-whisper-live-clean-transcript ()
  "Test transcript cleaning."
  (should (string= (whisper-live--clean-transcript "[noise] Hello world")
                   "Hello world"))
  (should (string= (whisper-live--clean-transcript "Hello (background) world")
                   "Hello  world"))
  (should (string= (whisper-live--clean-transcript "whisper_print_progress_callback: progress = 50% Hello")
                   "Hello"))
  (should (string= (whisper-live--clean-transcript "[BLANK_AUDIO]")
                   ""))
  (should (string= (whisper-live--clean-transcript "")
                   "")))

;;; Command Generation Tests

(ert-deftest test-whisper-live-command-without-prompt ()
  "Test whisper command generation without initial prompt."
  (let ((whisper-live-initial-prompt "")
        (test-file "/tmp/test.wav"))
    (let ((cmd (whisper-live--command test-file)))
      (should (listp cmd))
      (should (member "--file" cmd))
      (should (not (member "--prompt" cmd))))))

(ert-deftest test-whisper-live-command-with-prompt ()
  "Test whisper command generation with initial prompt."
  (let ((whisper-live-initial-prompt "Emacs, elisp, Python")
        (test-file "/tmp/test.wav"))
    (let ((cmd (whisper-live--command test-file)))
      (should (listp cmd))
      (should (member "--file" cmd))
      (should (member "--prompt" cmd))
      (should (member "Emacs, elisp, Python" cmd)))))

(ert-deftest test-whisper-live-command-remote ()
  "Test remote transcription command generation."
  (let ((whisper-live-remote-host "compute.example")
        (whisper-live-remote-model "large-v3-turbo-q5_0")
        (whisper-live-remote-program "/tmp/whisper-live-remote")
        (whisper-language "en")
        (test-file "/tmp/test.wav"))
    (should
     (equal (whisper-live--command test-file)
            '("/tmp/whisper-live-remote"
              "--host" "compute.example"
              "--language" "en"
              "--model" "large-v3-turbo-q5_0"
              "--file" "/tmp/test.wav")))))

(ert-deftest test-whisper-live-format-numbered-chunk ()
  "Test numbered chunk formatting."
  (let ((whisper-live-number-chunks t)
        (whisper-live-chunk-format "[%03d] %s\n"))
    (should (equal (whisper-live--format-chunk 7 "Hello.")
                   "[007] Hello.\n"))))

(ert-deftest test-whisper-live-format-unnumbered-chunk ()
  "Test sentence-style chunk formatting."
  (let ((whisper-live-number-chunks nil)
        (whisper-live-unnumbered-separator " "))
    (should (equal (whisper-live--format-chunk 7 "Hello.")
                   "Hello. "))))

(ert-deftest test-whisper-live-replace-provisional-buffer-text ()
  "A contextual transcription should revise, not append to, buffer text."
  (with-temp-buffer
    (insert "Prompt: ")
    (let ((whisper-live--insert-marker (point-marker))
          (whisper-live--insert-end-marker (point-marker))
          (whisper-live-number-chunks nil)
          (whisper-live-unnumbered-separator " ")
          (whisper-live-insert-prefix nil)
          (whisper-live-clean-with-llm nil))
      (whisper-live--replace-chunk-buffer 1 "First version.")
      (should (equal (buffer-string) "Prompt: First version. "))
      (whisper-live--replace-chunk-buffer 2 "A more natural revision.")
      (should (equal (buffer-string)
                     "Prompt: A more natural revision. ")))))

(ert-deftest test-whisper-live-replace-provisional-vterm-text ()
  "Vterm revision should erase exactly the prior provisional input."
  (let ((whisper-live--last-sent-length 4)
        (whisper-live-number-chunks nil)
        (whisper-live-unnumbered-separator " ")
        (whisper-live-insert-prefix nil)
        (backspaces 0)
        sent)
    (cl-letf (((symbol-function 'get-buffer-process)
               (lambda (_buffer) 'mock-process))
              ((symbol-function 'vterm-send-backspace)
               (lambda () (setq backspaces (1+ backspaces))))
              ((symbol-function 'vterm-send-string)
               (lambda (text &optional _paste-p) (setq sent text))))
      (whisper-live--replace-chunk-vterm 2 "Revised.")
      (should (= backspaces 4))
      (should (equal sent "Revised. "))
      (should (= whisper-live--last-sent-length (length "Revised. "))))))

(ert-deftest test-whisper-live-accumulative-handler-revises-text ()
  "Accumulation should replace an earlier hypothesis with the full revision."
  (with-temp-buffer
    (let ((whisper-live--insert-marker (point-marker))
          (whisper-live--insert-end-marker (point-marker))
          (whisper-live-transcription-mode 'accumulative)
          (whisper-live-accumulative-revise-text t)
          (whisper-live-number-chunks nil)
          (whisper-live-unnumbered-separator " ")
          (whisper-live-insert-prefix nil)
          (whisper-live-clean-with-llm nil)
          (whisper-live-voice-commands-enabled nil)
          (whisper-live--canceling nil)
          (whisper-live--chunk-id 0)
          (whisper-live--chunks nil)
          (whisper-live-transcribe-hook nil))
      (cl-letf (((symbol-function 'whisper--live-remove-tags) #'identity))
        (whisper-live--handle-transcription "I am work on it.")
        (whisper-live--handle-transcription "I am working on it."))
      (should (equal (buffer-string) "I am working on it. "))
      (should (= whisper-live--chunk-id 2)))))

;;; Queue Tests

(ert-deftest test-whisper-live-transcription-queue-init ()
  "Test transcription queue initialization."
  (let ((whisper-live--transcription-queue nil))
    (should (null whisper-live--transcription-queue))))

(ert-deftest test-whisper-live-current-transcription-init ()
  "Test current transcription initialization."
  (let ((whisper-live--current-transcription nil))
    (should (null whisper-live--current-transcription))))

;;; Word-based Truncation Tests (New Feature)

(ert-deftest test-whisper-live-extract-last-words-basic ()
  "Test extracting last N words from text."
  (should (string= (whisper-live--extract-last-words "one two three four five" 3)
                   "three four five"))
  (should (string= (whisper-live--extract-last-words "hello world" 1)
                   "world")))

(ert-deftest test-whisper-live-extract-last-words-fewer-than-limit ()
  "Test when text has fewer words than the limit."
  (should (string= (whisper-live--extract-last-words "one two" 5)
                   "one two"))
  (should (string= (whisper-live--extract-last-words "single" 10)
                   "single")))

(ert-deftest test-whisper-live-extract-last-words-empty-text ()
  "Test with empty or nil text."
  (should (null (whisper-live--extract-last-words "" 5)))
  (should (null (whisper-live--extract-last-words nil 5)))
  (should (null (whisper-live--extract-last-words "   " 5))))

(ert-deftest test-whisper-live-extract-last-words-whitespace ()
  "Test with text containing extra whitespace."
  (should (string= (whisper-live--extract-last-words "  one  two  three  " 2)
                   "two three")))

(ert-deftest test-whisper-live-extract-last-words-nil-limit ()
  "Test with nil limit (should return full text)."
  (let ((text "this is a longer sentence with many words"))
    (should (string= (whisper-live--extract-last-words text nil)
                     text))))

(ert-deftest test-whisper-live-extract-last-words-uses-config ()
  "Test that function respects whisper-live-display-words configuration."
  (let ((whisper-live-display-words 3))
    (should (string= (whisper-live--extract-last-words "one two three four five")
                     "three four five"))))

(ert-deftest test-whisper-live-extract-last-words-explicit-override ()
  "Test that explicit num-words parameter overrides configuration."
  (let ((whisper-live-display-words 10))
    (should (string= (whisper-live--extract-last-words "one two three four five" 2)
                     "four five"))))

(provide 'test-whisper-live-transcribe)

(when (not load-file-name)
  (message "test-whisper-live-transcribe.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; test-whisper-live-transcribe.el ends here
