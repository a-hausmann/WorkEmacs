;;; early-init.el
;;
;; Author:    Arnold Hausmann
;; Created:   2022-08-25
;; Email:     arnoldh@comcast.net, aehjr1@gmail.com
;;
;;

;; Set first message to mark Emacs startup timestamp.
(message (concat (format-time-string "[%F %T.%3N] ") "early-init.el started."))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar we/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun we/revert-file-name-handler-alist ()
  (setq file-name-handler-alist we/file-name-handler-alist))

(defun we/reset-gc ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'we/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'we/reset-gc)

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 64 1024))
  (setq w32-get-true-file-attributes nil)
  )
(when (boundp 'w32-pipe-buffer-size)
  (setq w32-pipe-buffer-size (* 64 1024)))

(setq-default
 load-prefer-newer t
 package-enable-at-startup nil)

(defun we/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
    Activate this advice with:
      (advice-add 'message :before 'aeh--ad-timestamp-message)
    Deactivate this advice with:
      (advice-remove 'message 'aeh--ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (format-time-string "[%F %T.%3N] "))))))
(advice-add 'message :before 'we/ad-timestamp-message)

;; 2021-02-22: Added below from ref: https://github.com/syl20bnr/spacemacs/issues/13866
(setq package-check-signature nil)

(message "early-init.el completed.")
