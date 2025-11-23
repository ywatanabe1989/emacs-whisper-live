;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-24 05:51:38>
;;; File: /home/ywatanabe/.emacs.d/lisp/whisper-live/src/whisper-live-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(require 'whisper)

(defvar whisper-live--transcription-text ""
  "Current transcription text.")

(defvar whisper-live-chunk-duration 5
  "Duration of each audio chunk in seconds.")

(defvar whisper-live-beep-on-start t
  "Beep when recording starts.")

(defvar whisper-live-beep-on-chunk t
  "Beep when each chunk is recorded.")

(defvar whisper-live-beep-on-stop t
  "Beep when recording stops.")

(defvar whisper-live-beep-start-frequency 1000
  "Frequency in Hz for start beep (higher = more distinct).")

(defvar whisper-live-beep-chunk-frequency 750
  "Frequency in Hz for chunk beep (middle tone).")

(defvar whisper-live-beep-stop-frequency 500
  "Frequency in Hz for stop beep (lower = ending tone).")

(defvar whisper-live-buzzer-script
  (expand-file-name "../docs/to_claude/bin/general/wsl2-buzzer.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WSL2 buzzer script for different frequency beeps.")

(defvar whisper-live-beep-start-wav
  (expand-file-name "start_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for start beep.")

(defvar whisper-live-beep-during-wav
  (expand-file-name "during_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for chunk/during beep.")

(defvar whisper-live-beep-stop-wav
  (expand-file-name "stop_beep.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to WAV file for stop beep.")

(defvar whisper-live-use-wav-beeps t
  "Use WAV files for beeps instead of frequency-based beeps.")

(defvar whisper-live-display-words 15
  "Number of words to display from each transcription chunk.
Set to nil to display full text.")

;; Auto-stop configuration
(defvar whisper-live-auto-stop-on-idle t
  "Enable automatic stop when Emacs is idle or silent.")

(defvar whisper-live-idle-timeout 5
  "Minutes of Emacs idle time before auto-stopping transcription.")

(defvar whisper-live-silence-timeout 60
  "Seconds of silence (no transcription) before auto-stopping.")

(defvar whisper-live-max-session-duration 30
  "Maximum session duration in minutes. Hard stop after this time.
Set to nil to disable hard stop.")

(defvar whisper-live--idle-timer nil
  "Timer for detecting Emacs idle state.")

(defvar whisper-live--silence-timer nil
  "Timer for checking silence duration.")

(defvar whisper-live--session-timer nil
  "Timer for maximum session duration hard stop.")

(defvar whisper-live--last-activity-time nil
  "Time of last transcription activity.")

(defvar whisper-live--session-start-time nil
  "Time when recording session started.")

(defvar whisper-live--current-process nil
  "Current recording process.")

(defvar whisper-live--chunks-directory nil
  "Directory to store temporary audio chunks.")

(defvar whisper-live--target-buffer nil
  "Buffer where transcribed text should be inserted.")

(defvar whisper-live--insert-marker nil
  "Marker for insertion position.")

(defvar whisper-live--insert-end-marker nil
  "Marker for end of insertion position.")

(defvar whisper-live--max-chunks 6
  "Maximum number of transcription chunks to keep in history.
Reduced to 6 (~30 seconds of audio) to prevent whisper timeout on long audio files.")

(defvar whisper-live-transcribe-hook nil
  "Hook run after each transcription.")

(defvar whisper-live--transcription-queue nil
  "Queue of files waiting to be transcribed.")

(defvar whisper-live--current-transcription nil
  "Currently running transcription process.")

(defvar whisper-live--last-sent-length 0
  "Length of last text sent to terminal for backspacing.")

(defvar whisper-live--sentence-counter 0
  "Counter for numbering transcribed sentences.")

(defvar whisper-live--chunk-id 0
  "Incrementing ID for each transcription chunk.")

(defvar whisper-live--chunks nil
  "List of chunks as plists: (:id N :text \"...\" :raw \"...\" :time TIME).")

(defvar whisper-live--last-transcription-time nil
  "Time when last transcription started.")

(defvar whisper-live--transcription-duration 0
  "Duration of last transcription in seconds.")

(defvar whisper-live--canceling nil
  "Flag to indicate transcription is being canceled.")

;; Core functions

(defun whisper-live--cleanup (&optional canceling)
  "Clean up resources used by whisper-live.
If CANCELING is non-nil, set canceling flag to prevent insertions."
  (when canceling
    (setq whisper-live--canceling t))
  (when whisper-live--current-process
    (delete-process whisper-live--current-process))
  (when whisper-live--current-transcription
    (delete-process whisper-live--current-transcription))
  (when whisper-live--transcription-queue
    (setq whisper-live--transcription-queue nil))
  ;; Cancel auto-stop timers
  (whisper-live--cancel-auto-stop-timers)
  ;; Remove mode line indicator
  (whisper-live--remove-mode-line)
  (setq whisper-live--current-process nil
        whisper-live--current-transcription nil
        whisper-live--transcription-text nil
        whisper-live--last-sent-length 0
        whisper-live--sentence-counter 0
        whisper-live--chunk-id 0
        whisper-live--chunks nil
        whisper-live--last-activity-time nil
        whisper-live--session-start-time nil)
  (whisper-live--cleanup-markers)
  ;; Reset canceling flag after a short delay
  (when canceling
    (run-with-timer 0.1 nil
                    (lambda () (setq whisper-live--canceling nil)))))

(defun whisper-live--cleanup-markers ()
  "Clean up markers."
  (when (markerp whisper-live--insert-marker)
    (set-marker whisper-live--insert-marker nil))
  (when (markerp whisper-live--insert-end-marker)
    (set-marker whisper-live--insert-end-marker nil))
  (setq whisper-live--insert-marker nil
        whisper-live--insert-end-marker nil))

(defun whisper-live--play-wav (wav-file)
  "Play a WAV file using available audio player.
WAV-FILE is the path to the WAV file to play."
  (when (and wav-file (file-exists-p wav-file))
    (cond
     ;; Try aplay (ALSA player - common on Linux/WSL)
     ((executable-find "aplay")
      (start-process "whisper-live-beep" nil "aplay" "-q" wav-file))
     ;; Try ffplay (from ffmpeg)
     ((executable-find "ffplay")
      (start-process "whisper-live-beep" nil "ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet" wav-file))
     ;; Try mpv
     ((executable-find "mpv")
      (start-process "whisper-live-beep" nil "mpv" "--no-video" "--really-quiet" wav-file))
     ;; Try afplay (macOS)
     ((executable-find "afplay")
      (start-process "whisper-live-beep" nil "afplay" wav-file))
     ;; Fallback to system beep
     (t (beep)))))

(defun whisper-live--beep (frequency &optional duration repeat-count)
  "Play a beep at FREQUENCY Hz or use WAV file if enabled.
FREQUENCY determines which beep type: start (1000), chunk (750), or stop (500).
DURATION is beep duration in milliseconds (default: 200).
REPEAT-COUNT is number of times to beep (default: 1).
If `whisper-live-use-wav-beeps' is t, plays corresponding WAV file.
Otherwise uses wsl2-buzzer.sh if available, or falls back to system beep."
  (if whisper-live-use-wav-beeps
      ;; Use WAV files based on frequency
      (let ((wav-file
             (cond
              ((= frequency whisper-live-beep-start-frequency)
               whisper-live-beep-start-wav)
              ((= frequency whisper-live-beep-chunk-frequency)
               whisper-live-beep-during-wav)
              ((= frequency whisper-live-beep-stop-frequency)
               whisper-live-beep-stop-wav)
              (t nil))))
        (if wav-file
            (whisper-live--play-wav wav-file)
          (beep)))
    ;; Use frequency-based beeps
    (let ((dur (or duration 200))
          (repeats (or repeat-count 1)))
      (if (and (file-exists-p whisper-live-buzzer-script)
               (file-executable-p whisper-live-buzzer-script))
          ;; Use wsl2-buzzer.sh for frequency control
          (start-process "whisper-live-beep" nil
                        whisper-live-buzzer-script
                        "-f" (number-to-string frequency)
                        "-d" (number-to-string dur)
                        "-r" (number-to-string repeats)
                        "-i" "0.1")
        ;; Fallback to system beep
        (dotimes (_ repeats)
          (beep)
          (when (> repeats 1)
            (sit-for 0.1)))))))

;; Auto-stop timer functions

(defun whisper-live--cancel-auto-stop-timers ()
  "Cancel all auto-stop timers."
  (when whisper-live--idle-timer
    (cancel-timer whisper-live--idle-timer)
    (setq whisper-live--idle-timer nil))
  (when whisper-live--silence-timer
    (cancel-timer whisper-live--silence-timer)
    (setq whisper-live--silence-timer nil))
  (when whisper-live--session-timer
    (cancel-timer whisper-live--session-timer)
    (setq whisper-live--session-timer nil)))

(defun whisper-live--auto-stop-on-idle ()
  "Auto-stop transcription due to Emacs idle."
  (when (and whisper-live--current-process
             ;; Don't stop if transcription is in progress
             (not whisper-live--current-transcription))
    (message "[whisper-live] Auto-stopping due to Emacs idle (%d min)"
             whisper-live-idle-timeout)
    (whisper-live--cleanup)
    (when whisper-live-beep-on-stop
      (whisper-live--beep whisper-live-beep-stop-frequency 300 1))))

(defun whisper-live--auto-stop-on-silence ()
  "Auto-stop transcription due to prolonged silence."
  (when (and whisper-live--current-process
             whisper-live--last-activity-time
             ;; Don't stop if transcription is in progress
             (not whisper-live--current-transcription))
    (let ((silence-duration (float-time
                             (time-subtract (current-time)
                                          whisper-live--last-activity-time))))
      (when (> silence-duration whisper-live-silence-timeout)
        (message "[whisper-live] Auto-stopping due to silence (%.0f sec)"
                 silence-duration)
        (whisper-live--cleanup)
        (when whisper-live-beep-on-stop
          (whisper-live--beep whisper-live-beep-stop-frequency 300 1))))))

(defun whisper-live--auto-stop-on-max-duration ()
  "Auto-stop transcription due to maximum session duration."
  (when whisper-live--current-process
    (message "[whisper-live] Auto-stopping: max session duration reached (%d min)"
             whisper-live-max-session-duration)
    (whisper-live--cleanup)
    (when whisper-live-beep-on-stop
      (whisper-live--beep whisper-live-beep-stop-frequency 300 1))))

(defun whisper-live--check-silence ()
  "Check for silence and auto-stop if needed.
This is called periodically to check silence duration."
  (when (and whisper-live-auto-stop-on-idle
             whisper-live--current-process)
    (whisper-live--auto-stop-on-silence)))

(defun whisper-live--start-auto-stop-timers ()
  "Start auto-stop timers if enabled."
  (when whisper-live-auto-stop-on-idle
    ;; Cancel any existing timers first
    (whisper-live--cancel-auto-stop-timers)

    ;; Initialize activity tracking
    (setq whisper-live--last-activity-time (current-time)
          whisper-live--session-start-time (current-time))

    ;; Start idle timer (triggers when Emacs is idle)
    (setq whisper-live--idle-timer
          (run-with-idle-timer (* whisper-live-idle-timeout 60)
                              nil
                              #'whisper-live--auto-stop-on-idle))

    ;; Start periodic silence checker (every 10 seconds)
    (setq whisper-live--silence-timer
          (run-with-timer 10 10 #'whisper-live--check-silence))

    ;; Start max session duration timer if configured
    (when whisper-live-max-session-duration
      (setq whisper-live--session-timer
            (run-with-timer (* whisper-live-max-session-duration 60)
                           nil
                           #'whisper-live--auto-stop-on-max-duration)))

    ;; Silently enable auto-stop timers
    nil))

(defun whisper-live--update-activity-time ()
  "Update last activity time when transcription occurs."
  (setq whisper-live--last-activity-time (current-time)))

;; Language switcher

(defvar whisper-live-supported-languages
  '(("auto" . "Auto-detect")
    ("en" . "English")
    ("ja" . "Japanese")
    ("zh" . "Chinese")
    ("de" . "German")
    ("es" . "Spanish")
    ("ru" . "Russian")
    ("ko" . "Korean")
    ("fr" . "French")
    ("pt" . "Portuguese")
    ("tr" . "Turkish")
    ("pl" . "Polish")
    ("ca" . "Catalan")
    ("nl" . "Dutch")
    ("ar" . "Arabic")
    ("sv" . "Swedish")
    ("it" . "Italian")
    ("id" . "Indonesian")
    ("hi" . "Hindi")
    ("fi" . "Finnish")
    ("vi" . "Vietnamese")
    ("he" . "Hebrew")
    ("uk" . "Ukrainian")
    ("el" . "Greek")
    ("ms" . "Malay")
    ("cs" . "Czech")
    ("ro" . "Romanian")
    ("da" . "Danish")
    ("hu" . "Hungarian")
    ("ta" . "Tamil")
    ("no" . "Norwegian")
    ("th" . "Thai")
    ("ur" . "Urdu")
    ("hr" . "Croatian")
    ("bg" . "Bulgarian")
    ("lt" . "Lithuanian")
    ("la" . "Latin")
    ("mi" . "Maori")
    ("ml" . "Malayalam")
    ("cy" . "Welsh")
    ("sk" . "Slovak")
    ("te" . "Telugu")
    ("fa" . "Persian")
    ("lv" . "Latvian")
    ("bn" . "Bengali")
    ("sr" . "Serbian")
    ("az" . "Azerbaijani")
    ("sl" . "Slovenian")
    ("kn" . "Kannada")
    ("et" . "Estonian")
    ("mk" . "Macedonian")
    ("br" . "Breton")
    ("eu" . "Basque")
    ("is" . "Icelandic")
    ("hy" . "Armenian")
    ("ne" . "Nepali")
    ("mn" . "Mongolian")
    ("bs" . "Bosnian")
    ("kk" . "Kazakh")
    ("sq" . "Albanian")
    ("sw" . "Swahili")
    ("gl" . "Galician")
    ("mr" . "Marathi")
    ("pa" . "Punjabi")
    ("si" . "Sinhala")
    ("km" . "Khmer")
    ("sn" . "Shona")
    ("yo" . "Yoruba")
    ("so" . "Somali")
    ("af" . "Afrikaans")
    ("oc" . "Occitan")
    ("ka" . "Georgian")
    ("be" . "Belarusian")
    ("tg" . "Tajik")
    ("sd" . "Sindhi")
    ("gu" . "Gujarati")
    ("am" . "Amharic")
    ("yi" . "Yiddish")
    ("lo" . "Lao")
    ("uz" . "Uzbek")
    ("fo" . "Faroese")
    ("ht" . "Haitian Creole")
    ("ps" . "Pashto")
    ("tk" . "Turkmen")
    ("nn" . "Nynorsk")
    ("mt" . "Maltese")
    ("sa" . "Sanskrit")
    ("lb" . "Luxembourgish")
    ("my" . "Myanmar")
    ("bo" . "Tibetan")
    ("tl" . "Tagalog")
    ("mg" . "Malagasy")
    ("as" . "Assamese")
    ("tt" . "Tatar")
    ("haw" . "Hawaiian")
    ("ln" . "Lingala")
    ("ha" . "Hausa")
    ("ba" . "Bashkir")
    ("jw" . "Javanese")
    ("su" . "Sundanese"))
  "Alist of supported languages for whisper transcription.
Format: (CODE . NAME) where CODE is ISO 639-1 code and NAME is display name.")

(defvar whisper-live-languages '("en" "ja" "auto")
  "List of languages to cycle through with `whisper-live-switch-language'.
Default is English (en), Japanese (ja), and auto-detect (auto).
You can customize this list with codes from `whisper-live-supported-languages'.")

(defvar whisper-live-default-language "ja"
  "Default language for whisper-live transcription.
Set to 'ja' for Japanese, 'en' for English, or 'auto' for automatic detection.")

(defvar whisper-live-debug-output nil
  "Enable debug output for transcription.
When t, saves raw whisper output to /tmp/whisper-live-debug-*.txt files.")

;; Mode line indicator
(defvar whisper-live-mode-line-format '(:eval (whisper-live--mode-line-string))
  "Mode line format for whisper-live.")

(defvar whisper-live-show-in-mode-line t
  "Show recording status in mode line when non-nil.")

(defun whisper-live--mode-line-string ()
  "Generate mode line string for whisper-live status."
  (when (and whisper-live-show-in-mode-line
             whisper-live--current-process)
    (let ((lang-str (cond
                     ((string-equal whisper-language "auto") "🌐")
                     ((string-equal whisper-language "en") "EN")
                     ((string-equal whisper-language "ja") "JA")
                     (t (upcase whisper-language))))
          (transcribing (when whisper-live--current-transcription "📝")))
      (propertize (format " 🎤%s%s " lang-str (or transcribing ""))
                  'face '(:foreground "red" :weight bold)
                  'help-echo (format "Whisper Live: Recording (%s)" whisper-language)))))

(defun whisper-live--add-mode-line ()
  "Add whisper-live indicator to mode line."
  (when whisper-live-show-in-mode-line
    (unless (member whisper-live-mode-line-format global-mode-string)
      (setq global-mode-string
            (append global-mode-string (list whisper-live-mode-line-format)))
      (force-mode-line-update t))))

(defun whisper-live--remove-mode-line ()
  "Remove whisper-live indicator from mode line."
  (setq global-mode-string
        (remove whisper-live-mode-line-format global-mode-string))
  (force-mode-line-update t))

(defun whisper-live-select-language ()
  "Select language for whisper transcription with completion.
Offers all supported languages from `whisper-live-supported-languages'."
  (interactive)
  (let* ((current whisper-language)
         (current-name (or (cdr (assoc current whisper-live-supported-languages))
                          current))
         ;; Create completion candidates with format "code - Name"
         (candidates (mapcar (lambda (lang)
                              (cons (format "%s - %s" (car lang) (cdr lang))
                                    (car lang)))
                            whisper-live-supported-languages))
         (prompt (format "Select language [current: %s - %s]: "
                        current current-name))
         (selection (completing-read prompt candidates nil t))
         (new-lang (cdr (assoc selection candidates))))
    (when new-lang
      (setq whisper-language new-lang)
      ;; Warn if using .en model with non-English language
      (when (and (not (string-equal new-lang "en"))
                 (not (string-equal new-lang "auto"))
                 (string-suffix-p ".en" whisper-model))
        (warn "[whisper-live] WARNING: Using .en model (%s) with %s language. \
This may cause empty transcriptions. Use generic model (without .en) instead."
              whisper-model new-lang))
      (message "[whisper-live] Language switched: %s -> %s (model: %s%s)"
               current new-lang whisper-model
               (if whisper-quantize (concat "-" whisper-quantize) ""))
      (force-mode-line-update t)
      new-lang)))

(defun whisper-live-cycle-language ()
  "Quickly cycle through languages in `whisper-live-languages' list.
This is the fast toggle function - no prompts, instant switching.
Cycles: en → ja → auto (or your custom list)."
  (interactive)
  (let* ((current whisper-language)
         (current-idx (cl-position current whisper-live-languages :test #'string=))
         (next-idx (if current-idx
                      (mod (1+ current-idx) (length whisper-live-languages))
                    0))
         (next-lang (nth next-idx whisper-live-languages)))
    (setq whisper-language next-lang)
    ;; Warn if using .en model with non-English language
    (when (and (not (string-equal next-lang "en"))
               (not (string-equal next-lang "auto"))
               (string-suffix-p ".en" whisper-model))
      (warn "[whisper-live] WARNING: Using .en model (%s) with %s language. \
This may cause empty transcriptions. Use generic model (without .en) instead."
            whisper-model next-lang))
    (message "[whisper-live] Language: %s -> %s"
             current next-lang)
    (force-mode-line-update t)
    next-lang))

(defun whisper-live-switch-language (&optional use-completion)
  "Switch language for whisper transcription.
By default, quickly cycles through languages in `whisper-live-languages' list.
With prefix argument USE-COMPLETION, offers interactive selection with completion
from all 99 supported languages.

Fast toggle (default): en → ja → auto
Full selection (C-u): Choose from all languages with completion"
  (interactive "P")
  (if use-completion
      ;; Interactive selection with completion
      (whisper-live-select-language)
    ;; Quick cycle through predefined list (FAST)
    (whisper-live-cycle-language)))

(defvar whisper-live--last-key-press-time nil
  "Time of last key press for double-press detection.")

(defvar whisper-live-double-press-timeout 0.3
  "Timeout in seconds for detecting double-press (default: 0.3).
If key is pressed twice within this time, cycle language instead of toggling recording.")

(defun whisper-live-smart-toggle ()
  "Smart toggle: single press starts/stops recording, double press cycles language.
- Single press (or first press): Start/stop whisper-live recording
- Double press within `whisper-live-double-press-timeout': Cycle language

Bind this to a key like Alt-Enter for quick access."
  (interactive)
  (let ((current-time (float-time))
        (last-time whisper-live--last-key-press-time))
    (if (and last-time
             (< (- current-time last-time) whisper-live-double-press-timeout))
        ;; Double press detected - cycle language
        (progn
          (setq whisper-live--last-key-press-time nil)  ; Reset
          (whisper-live-cycle-language))
      ;; Single press - toggle recording
      (setq whisper-live--last-key-press-time current-time)
      (run-with-timer whisper-live-double-press-timeout nil
                      (lambda ()
                        (when (equal whisper-live--last-key-press-time current-time)
                          ;; No second press came, execute toggle
                          (whisper-live-run)
                          (setq whisper-live--last-key-press-time nil)))))))

(defvar whisper-live--initialized nil
  "Flag to track if whisper-live has been initialized.")

(defun whisper-live--init ()
  "Initialize whisper-live settings and directories."
  (whisper-live--generate-chunks-directory)
  ;; Ensure language is valid for whisper-live
  (when (and (not (member whisper-language whisper-live-languages))
             (not (string-equal whisper-language "auto")))
    (setq whisper-language whisper-live-default-language)
    (message "[whisper-live] Language changed to: %s" whisper-language))
  (when (not whisper-live--initialized)
    ;; Initialize paths
    (setq whisper--install-path (expand-file-name
                                 "whisper.cpp/"
                                 (file-name-as-directory
                                  whisper-install-directory))
          whisper--temp-file
          (expand-file-name "emacs-whisper.wav"
                            temporary-file-directory)
          ;; Initialize process variables
          whisper--recording-process nil
          whisper--transcribing-process nil
          whisper--using-whispercpp nil
          whisper--progress-level "0"
          whisper--ffmpeg-input-file nil
          ;; Initialize buffers and markers
          whisper--point-buffer (current-buffer)
          whisper--marker (make-marker)
          ;; Initialize live transcription variables
          whisper-live--transcription-queue nil
          whisper-live--current-transcription nil
          whisper-live--transcription-text nil
          whisper-live--last-sent-length 0
          whisper-live--sentence-counter 0
          whisper-live--chunk-id 0
          whisper-live--chunks nil
          whisper-live--canceling nil
          whisper-live--insert-marker (point-marker)
          whisper-live--insert-end-marker (point-marker))

    ;; Suppress warnings by setting mode line
    (when whisper-show-progress-in-mode-line
      (setq global-mode-string
            (remove '(t ,(whisper--mode-line-indicator 'recording))
                    global-mode-string)
            global-mode-string
            (remove '(t ,(whisper--mode-line-indicator 'transcribing))
                    global-mode-string)))

    (setq whisper-live--initialized t)))

(defun whisper-live--cleanup-on-quit ()
  "Cleanup when user cancels with C-g."
  (when whisper-live--current-process
    (whisper-live--cleanup t)))

(add-hook 'keyboard-quit-hook #'whisper-live--cleanup-on-quit)


(provide 'whisper-live-core)

(when
    (not load-file-name)
  (message "whisper-live-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))