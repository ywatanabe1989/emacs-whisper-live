;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 05:51:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-audio.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


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

(defvar whisper-live-show-volume-indicator t
  "When non-nil, show visual volume indicator in minibuffer.")

(defun whisper-live--get-audio-volume (audio-file)
  "Get mean and max volume of AUDIO-FILE in dB using ffmpeg.
Returns a plist (:mean MEAN :max MAX) or nil if detection fails."
  (when (and audio-file (file-exists-p audio-file))
    (let ((output (with-temp-buffer
                    (call-process "ffmpeg" nil t nil
                                  "-i" audio-file
                                  "-af" "volumedetect"
                                  "-f" "null" "-"
                                  "-hide_banner")
                    (buffer-string)))
          mean-vol max-vol)
      ;; Parse mean_volume from ffmpeg output
      ;; Example: [Parsed_volumedetect_0 @ ...] mean_volume: -25.3 dB
      (when (string-match "mean_volume: \\(-?[0-9.]+\\) dB" output)
        (setq mean-vol (string-to-number (match-string 1 output))))
      ;; Parse max_volume (peak)
      ;; Example: [Parsed_volumedetect_0 @ ...] max_volume: -5.2 dB
      (when (string-match "max_volume: \\(-?[0-9.]+\\) dB" output)
        (setq max-vol (string-to-number (match-string 1 output))))
      (when (or mean-vol max-vol)
        (list :mean mean-vol :max max-vol)))))

(defun whisper-live--volume-to-bar (volume-db &optional width)
  "Convert VOLUME-DB to a text-based volume bar.
WIDTH is the bar width in characters (default 20).
Returns a string like: [████████░░░░░░░░░░░░] -25dB"
  (let* ((width (or width 20))
         ;; Map dB to 0-100 range: -60dB=0%, 0dB=100%
         (min-db -60.0)
         (max-db 0.0)
         (clamped-vol (max min-db (min max-db (or volume-db min-db))))
         (percent (/ (- clamped-vol min-db) (- max-db min-db)))
         (filled (round (* width percent)))
         (empty (- width filled))
         (bar (concat (make-string filled ?█)
                      (make-string empty ?░))))
    (format "[%s] %+.0fdB" bar (or volume-db -99))))

(defun whisper-live--show-volume-indicator (vol-info)
  "Display volume indicator in minibuffer from VOL-INFO plist."
  (when (and whisper-live-show-volume-indicator vol-info)
    (let* ((mean-vol (plist-get vol-info :mean))
           (max-vol (plist-get vol-info :max))
           (mean-bar (whisper-live--volume-to-bar mean-vol))
           (status (cond
                    ((and mean-vol (> mean-vol -20)) "LOUD")
                    ((and mean-vol (> mean-vol -35)) "good")
                    ((and mean-vol (> mean-vol -50)) "quiet")
                    (t "silent"))))
      (message "[whisper-live] Volume: %s (%s) peak:%+.0fdB"
               mean-bar status (or max-vol -99)))))

(defun whisper-live--update-volume-stats (mean-vol)
  "Update background noise statistics with MEAN-VOL measurement."
  (when mean-vol
    ;; Add to history
    (push mean-vol whisper-live--volume-history)
    ;; Trim to max size
    (when (> (length whisper-live--volume-history) whisper-live--volume-history-max)
      (setq whisper-live--volume-history
            (seq-take whisper-live--volume-history whisper-live--volume-history-max)))
    ;; Recalculate statistics (only from quieter samples - likely background)
    (when (>= (length whisper-live--volume-history) 3)
      (let* ((sorted (sort (copy-sequence whisper-live--volume-history) #'<))
             ;; Use lower 60% of samples as background noise estimate
             (bg-samples (seq-take sorted (max 3 (/ (* (length sorted) 60) 100))))
             (n (length bg-samples))
             (sum (apply #'+ bg-samples))
             (mean (/ sum (float n)))
             (variance (/ (apply #'+ (mapcar (lambda (x) (expt (- x mean) 2)) bg-samples))
                         (float n)))
             (stddev (sqrt variance)))
        (setq whisper-live--background-mean mean
              whisper-live--background-stddev (max stddev 1.0)))))) ; min stddev of 1 dB

(defun whisper-live--chunk-has-speech-p (audio-file)
  "Check if AUDIO-FILE has speech using adaptive or fixed threshold.
When `whisper-live-adaptive-threshold' is non-nil, uses z-score based
detection that learns from background noise and detects peaks.
Otherwise uses fixed `whisper-live-volume-threshold'."
  (if (not whisper-live-skip-quiet-chunks)
      t  ; Always process if threshold check disabled
    (let ((vol-info (whisper-live--get-audio-volume audio-file)))
      ;; Show visual volume indicator
      (whisper-live--show-volume-indicator vol-info)
      (if vol-info
          (let* ((mean-vol (plist-get vol-info :mean))
                 (max-vol (plist-get vol-info :max)))
            (if whisper-live-adaptive-threshold
                ;; Adaptive z-score + peak detection
                (progn
                  (whisper-live--update-volume-stats mean-vol)
                  (let* ((zscore (if whisper-live--background-mean
                                    (/ (- mean-vol whisper-live--background-mean)
                                       whisper-live--background-stddev)
                                  0))
                         (peak-zscore (if (and max-vol whisper-live--background-mean)
                                         (/ (- max-vol whisper-live--background-mean)
                                            whisper-live--background-stddev)
                                       0))
                         (peak-diff (when max-vol (- max-vol mean-vol)))
                         (has-peaks (and peak-diff (> peak-diff 5)))
                         (has-speech
                          (cond
                           ;; Not enough history yet - use fallback
                           ((or (null whisper-live--background-mean)
                                (< (length whisper-live--volume-history) 3))
                            (>= mean-vol whisper-live-volume-threshold))
                           ;; LENIENT detection for quiet voices:
                           ;; 1. Mean z-score >= threshold, OR
                           ;; 2. Has peaks AND mean z-score >= 0.5, OR
                           ;; 3. Peak z-score >= 1.0 (peak is 1 SD above background)
                           (t (or (>= zscore whisper-live-zscore-threshold)
                                  (and has-peaks (>= zscore 0.5))
                                  (>= peak-zscore 1.0))))))
                    (unless has-speech
                      (message "[whisper-live] Skipping (mean=%.1f, peak=%.1f, bg=%.1f±%.1f, z=%.1f, pz=%.1f)"
                               mean-vol (or max-vol -99)
                               (or whisper-live--background-mean 0)
                               (or whisper-live--background-stddev 0)
                               zscore peak-zscore))
                    has-speech))
              ;; Fixed threshold mode
              (let ((has-speech (>= mean-vol whisper-live-volume-threshold)))
                (unless has-speech
                  (message "[whisper-live] Skipping quiet chunk (%.1f dB < %.1f dB threshold)"
                           mean-vol whisper-live-volume-threshold))
                has-speech)))
        ;; If volume detection fails, process anyway
        t))))

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