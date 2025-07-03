;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-04 08:52:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-audio.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Time-stamp: <2024-12-08 18:06:39 (ywatanabe)>

(require 'whisper-live-core)

(defun whisper-live--generate-chunks-directory ()
  "Generate new directory path for audio chunks."
  (concat temporary-file-directory
          (format-time-string "whisper-live-chunks/%Y%m%d-%H%M%S/")))

(defun whisper-live--update-chunks-directory ()
  "Update the chunks directory with a new timestamp."
  (setq whisper-live--chunks-directory
        (whisper-live--generate-chunks-directory))
  (make-directory whisper-live--chunks-directory t))

(defun whisper-live--ensure-directory ()
  "Ensure chunks directory exists."
  (unless (file-exists-p whisper-live--chunks-directory)
    (make-directory whisper-live--chunks-directory t)))

(defun whisper-live--generate-chunk-filename ()
  "Generate unique filename for audio chunk."
  (format "%swhisper-chunk-%s.wav"
          whisper-live--chunks-directory
          (format-time-string "%Y%m%d-%H%M%S")))

(defun whisper-live--concatenate-chunks (chunk-directory)
  "Concatenate recent wav chunks in CHUNK-DIRECTORY into single file."
  (let* ((output-file (concat chunk-directory "combined.wav"))
         (chunk-files
          (directory-files chunk-directory t "whisper-chunk-.*\\.wav$"))
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
      (let
          ((old-chunks
            (butlast sorted-chunks whisper-live--max-chunks)))
        (dolist (chunk old-chunks)
          (when (file-exists-p chunk)
            (delete-file chunk))))
      output-file)))


(provide 'whisper-live-audio)

(when
    (not load-file-name)
  (message "whisper-live-audio.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))