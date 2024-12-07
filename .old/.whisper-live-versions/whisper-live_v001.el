;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 08:49:05
;;; Time-stamp: <2024-12-07 08:49:05 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/whisper-live.el


;;; whisper-live.el --- Real-time speech transcription using Whisper -*- lexical-binding: t; -*-

(require 'whisper)

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

(defun whisper-live--ensure-directory ()
  "Ensure chunks directory exists."
  (unless (file-exists-p whisper-live--chunks-directory)
    (make-directory whisper-live--chunks-directory t)))

(defun whisper-live--generate-chunk-filename ()
  "Generate unique filename for audio chunk."
  (format "%swhisper-chunk-%s.wav"
          whisper-live--chunks-directory
          (format-time-string "%Y%m%d-%H%M%S")))

;; (defun whisper-live--transcribe-chunk (chunk-file)
;;   "Transcribe a single CHUNK-FILE."
;;   (make-process
;;    :name "whisper-live-transcribing"
;;    :command (whisper-command chunk-file)
;;    :buffer (get-buffer-create "*whisper-temp*")
;;    :sentinel (lambda (_process event)
;;                (when (string-equal "finished\n" event)
;;                  (with-current-buffer (get-buffer "*whisper-temp*")
;;                    (let ((text (buffer-string)))
;;                      (when (not (string-empty-p text))
;;                        (with-current-buffer (get-buffer-create whisper-live-buffer-name)
;;                          (goto-char (point-max))
;;                          (insert text "\n")))))
;;                  (kill-buffer "*whisper-temp*")
;;                  (delete-file chunk-file)))))

(defun whisper-live--transcribe-chunk (chunk-file)
  "Transcribe a single CHUNK-FILE."
  (make-process
   :name "whisper-live-transcribing"
   :command (whisper-command chunk-file)
   :buffer (get-buffer-create "*whisper-temp*")
   :sentinel (lambda (_process event)
               (when (string-equal "finished\n" event)
                 (with-current-buffer (get-buffer "*whisper-temp*")
                   (goto-char (point-min))
                   (when (re-search-forward "\n\n \\(.*\\)\n\n" nil t)
                     (let ((text (match-string 1)))
                       (with-current-buffer (get-buffer-create whisper-live-buffer-name)
                         (goto-char (point-max))
                         (insert text "\n")))))
                 (kill-buffer "*whisper-temp*")
                 (delete-file chunk-file)))))

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
  (whisper-live--ensure-directory)
  (get-buffer-create whisper-live-buffer-name)
  (display-buffer whisper-live-buffer-name)
  (whisper-live--record-chunk)
  (message "Started real-time transcription. Use whisper-live-stop to end."))

(defun whisper-live-stop ()
  "Stop real-time transcription."
  (interactive)
  (when whisper-live--current-process
    (delete-process whisper-live--current-process)
    (setq whisper-live--current-process nil))
  (when (file-exists-p whisper-live--chunks-directory)
    (delete-directory whisper-live--chunks-directory t))
  (message "Stopped real-time transcription."))

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


;; (whisper-live-run)

(provide 'whisper-live)
;;; whisper-live.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
