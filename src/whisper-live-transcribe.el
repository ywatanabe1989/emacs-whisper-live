;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-08 18:06:51
;;; Time-stamp: <2024-12-08 19:17:19 (ywatanabe)>
;;; File: ./whisper-live/src/whisper-live-transcribe.el


(require 'whisper-live-core)
(require 'whisper-live-audio)

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
    (let ((cleaned (replace-regexp-in-string "\\[.*?\\]\\|([^)]*)" "" text)))
      (string-trim cleaned))))

(defun whisper-live--transcribe-chunk (concatenated-file)
  "Transcribe a single CONCATENATED-FILE."
  (let ((cmd (whisper-command concatenated-file))
        (temp-buffer (generate-new-buffer " *whisper-temp*")))
    (when (and cmd (car cmd))
      (make-process
       :name "whisper-live-transcribing"
       :command cmd
       :buffer temp-buffer
       :sentinel (lambda (process event)
                   (let ((process-buffer (process-buffer process)))
                     (when (string-equal "finished\n" event)
                       (let ((text (with-current-buffer process-buffer
                                     (when (string-match "\n\n \\(.*\\)\n\n" (buffer-string))
                                       (match-string 1 (buffer-string))))))
                         (when (and text
                                    (not (string-empty-p (string-trim text)))
                                    (not (string-match-p "^[[:space:].,]*$" text)))
                           (setq whisper-live--transcription-text
                                 (whisper-live--clean-transcript text))
                           (with-current-buffer (marker-buffer whisper-live--insert-marker)
                             (save-excursion
                               (goto-char whisper-live--insert-marker)
                               (delete-region whisper-live--insert-marker whisper-live--insert-end-marker)
                               (let ((clean-text (whisper--live-remove-tags whisper-live--transcription-text)))
                                 (when whisper-live-clean-with-llm
                                   (insert whisper-live-start-tag))
                                 (insert clean-text)
                                 (when whisper-live-clean-with-llm
                                   (insert whisper-live-end-tag)))
                               (set-marker whisper-live--insert-end-marker (point))))
                           (run-hooks 'whisper-live-transcribe-hook)))
                       (setq whisper-live--current-transcription nil)
                       (whisper-live--process-transcription-queue)
                       (kill-buffer process-buffer))))))))

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
                         (let ((combined-file (whisper-live--concatenate-chunks whisper-live--chunks-directory)))
                           (push combined-file whisper-live--transcription-queue)
                           (whisper-live--process-transcription-queue))
                         (whisper-live--record-chunk)))))))

(provide 'whisper-live-transcribe)
