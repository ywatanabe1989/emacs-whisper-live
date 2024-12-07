;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 09:46:32
;;; Time-stamp: <2024-12-07 09:46:32 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/whisper-live.el


; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 09:38:55
;;; Time-stamp: <2024-12-07 09:38:55 (ywatanabe)>
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
   :buffer (get-buffer-create "*whisper-temp*")
   :sentinel (lambda (_process event)
               (when (string-equal "finished\n" event)
                 (with-current-buffer (get-buffer "*whisper-temp*")
                   (goto-char (point-min))
                   (when (re-search-forward "\n\n \\(.*\\)\n\n" nil t)
                     (let ((text (match-string 1)))
                       (when (and text (marker-buffer whisper-live--insert-marker))
                         (with-current-buffer (marker-buffer whisper-live--insert-marker)
                           (save-excursion
                             (goto-char whisper-live--insert-marker)
                             (insert text " ")
                             (set-marker whisper-live--insert-marker (point))))))))
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
  (setq whisper-live--target-buffer (current-buffer)
        whisper-live--insert-marker (point-marker))
  (whisper-live--ensure-directory)
  (whisper-live--record-chunk)
  (message "Live transcribing..."))

(defun whisper-live-stop ()
  "Stop real-time transcription."
  (interactive)
  (when whisper-live--current-process
    (delete-process whisper-live--current-process)
    (setq whisper-live--current-process nil))
  (when whisper-live--insert-marker
    (set-marker whisper-live--insert-marker nil))
  (when (file-exists-p whisper-live--chunks-directory)
    (delete-directory whisper-live--chunks-directory t))
  (message "[-] Stopped."))

(defun whisper-live-clear-buffer ()
  "Clear the transcription buffer."
  (interactive)
  (with-current-buffer (get-buffer-create whisper-live-buffer-name)
    (erase-buffer)))

(defun whisper-live--clean-transcript (text)
  "Clean transcript TEXT by removing parenthetical and bracket expressions."
  (when text
    (let ((cleaned (replace-regexp-in-string "\\[.*?\\]\\|\\(.*?\\)" "" text)))
      (string-trim cleaned))))

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

;; (whisper-live-run)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
