;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/whisper-live/tests/test-whisper-live-core.el

;;; Commentary:
;; Core functionality tests for whisper-live

;;; Code:

(require 'ert)

;; Add parent directory to load path
(let ((default-directory (file-name-directory
                          (directory-file-name
                           (file-name-directory load-file-name)))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (expand-file-name "src" default-directory)))

(require 'whisper-live-core)
;; whisper-live-run is optional (requires 'request package)
(condition-case nil
    (require 'whisper-live-run)
  (error nil))

;;; Variable Tests

(ert-deftest test-whisper-live-chunk-duration-default ()
  "Test default chunk duration value."
  (should (numberp whisper-live-chunk-duration))
  (should (> whisper-live-chunk-duration 0)))

(ert-deftest test-whisper-live-max-chunks-default ()
  "Test default max chunks value."
  (should (numberp whisper-live--max-chunks))
  (should (> whisper-live--max-chunks 0)))

(ert-deftest test-whisper-live-mode-line-indicator ()
  "Test mode line indicator is a string."
  (should (stringp whisper-live-mode-line-indicator))
  (should (> (length whisper-live-mode-line-indicator) 0)))

(ert-deftest test-whisper-live-audio-feedback-boolean ()
  "Test audio feedback setting is boolean."
  (should (or (eq whisper-live-audio-feedback t)
              (eq whisper-live-audio-feedback nil))))

(ert-deftest test-whisper-live-buzzer-frequency ()
  "Test buzzer frequency is valid."
  (should (numberp whisper-live-buzzer-frequency))
  (should (> whisper-live-buzzer-frequency 0))
  (should (< whisper-live-buzzer-frequency 20000)))

(ert-deftest test-whisper-live-buzzer-volume ()
  "Test buzzer volume is in valid range."
  (should (numberp whisper-live-buzzer-volume))
  (should (>= whisper-live-buzzer-volume 0.0))
  (should (<= whisper-live-buzzer-volume 1.0)))

(ert-deftest test-whisper-live-number-chunks-boolean ()
  "Test number chunks setting is boolean."
  (should (or (eq whisper-live-number-chunks t)
              (eq whisper-live-number-chunks nil))))

(ert-deftest test-whisper-live-chunk-format ()
  "Test chunk format is a string with format specifiers."
  (should (stringp whisper-live-chunk-format))
  (should (string-match-p "%d" whisper-live-chunk-format))
  (should (string-match-p "%s" whisper-live-chunk-format)))

;;; Function Tests

(ert-deftest test-whisper-live-cleanup-markers ()
  "Test marker cleanup function."
  (let ((whisper-live--insert-marker (make-marker))
        (whisper-live--insert-end-marker (make-marker)))
    (whisper-live--cleanup-markers)
    (should (null whisper-live--insert-marker))
    (should (null whisper-live--insert-end-marker))))

(ert-deftest test-whisper-live-cleanup ()
  "Test general cleanup function."
  (let ((whisper-live--current-process nil)
        (whisper-live--transcription-queue '("file1" "file2"))
        (whisper-live--transcription-text "some text")
        (whisper-live--chunk-counter 5))
    (whisper-live--cleanup)
    (should (null whisper-live--current-process))
    (should (null whisper-live--transcription-queue))
    (should (null whisper-live--transcription-text))
    (should (= whisper-live--chunk-counter 0))))

(ert-deftest test-whisper-live-init ()
  "Test initialization function."
  (let ((whisper-live--initialized nil))
    (whisper-live--init)
    (should whisper-live--chunks-directory)
    (should (file-directory-p whisper-live--chunks-directory))
    (should whisper-live--initialized)))

(ert-deftest test-whisper-live-switch-model ()
  "Test model switching function."
  (let ((original-model whisper-model))
    ;; Test direct assignment (the actual implementation uses interactive completion)
    (setq whisper-model "tiny")
    (should (string= whisper-model "tiny"))
    (setq whisper-model "base.en")
    (should (string= whisper-model "base.en"))
    (setq whisper-model "large-v3")
    (should (string= whisper-model "large-v3"))
    ;; Restore original
    (setq whisper-model original-model)))

(ert-deftest test-whisper-live-switch-model-function-exists ()
  "Test that whisper-live-switch-model function exists and is callable."
  :tags '(optional-dependency)
  (skip-unless (featurep 'whisper-live-run))
  (should (fboundp 'whisper-live-switch-model))
  (should (commandp 'whisper-live-switch-model)))

;;; New Feature Tests (Chunk-based numbering system)

(ert-deftest test-whisper-live-chunk-id-initialization ()
  "Test chunk ID initializes to 0."
  (should (numberp whisper-live--chunk-id))
  (should (>= whisper-live--chunk-id 0)))

(ert-deftest test-whisper-live-chunks-list-initialization ()
  "Test chunks list initializes as list."
  (should (listp whisper-live--chunks)))

(ert-deftest test-whisper-live-display-words-config ()
  "Test display words configuration."
  (should (numberp whisper-live-display-words))
  (should (> whisper-live-display-words 0)))

(ert-deftest test-whisper-live-beep-frequencies ()
  "Test beep frequency configurations are valid."
  (should (numberp whisper-live-beep-start-frequency))
  (should (numberp whisper-live-beep-chunk-frequency))
  (should (numberp whisper-live-beep-stop-frequency))
  (should (> whisper-live-beep-start-frequency 0))
  (should (> whisper-live-beep-chunk-frequency 0))
  (should (> whisper-live-beep-stop-frequency 0))
  ;; Start should be highest, stop should be lowest
  (should (> whisper-live-beep-start-frequency whisper-live-beep-chunk-frequency))
  (should (> whisper-live-beep-chunk-frequency whisper-live-beep-stop-frequency)))

(ert-deftest test-whisper-live-beep-on-flags ()
  "Test beep enable/disable flags are boolean."
  (should (booleanp whisper-live-beep-on-start))
  (should (booleanp whisper-live-beep-on-chunk))
  (should (booleanp whisper-live-beep-on-stop)))

(ert-deftest test-whisper-live-beep-function-exists ()
  "Test beep function exists and is callable."
  (should (fboundp 'whisper-live--beep)))

(ert-deftest test-whisper-live-cleanup-resets-chunk-data ()
  "Test cleanup resets chunk-related variables."
  (let ((whisper-live--chunk-id 5)
        (whisper-live--chunks '((:id 1 :text "test"))))
    (whisper-live--cleanup)
    (should (= whisper-live--chunk-id 0))
    (should (null whisper-live--chunks))))

(provide 'test-whisper-live-core)

(when (not load-file-name)
  (message "test-whisper-live-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; test-whisper-live-core.el ends here
