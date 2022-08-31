;; -*- lexical-binding: t -*-
;; File: we-requires.el
;; Last modified: Wed Aug 31, 2022 9:20:14
;; Purpose: This is the "master" configuration file, loading all others.

;; Set the size of the frame
(when window-system
  (if (string-equal system-type "windows-nt")
      (progn
        ;; 2020-07-14: while I'm on laptop-only, less size.
        ;; (add-to-list 'default-frame-alist '(height . 45))
        ;; (add-to-list 'default-frame-alist '(width . 150))
        (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 140)))
    (progn
      (add-to-list 'default-frame-alist '(height . 38))
      (add-to-list 'default-frame-alist '(width . 140))))
  (blink-cursor-mode 0))

;; Set the Customization file
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set garbage collection hook
(add-hook 'focus-out-hook #'garbage-collect)

;; Set regular package refreshes without killing restarts
(defvar last-package-refresh-file (expand-file-name ".we-last-package-refresh-file" user-emacs-directory))
(defun we/package-refresh-contents (mess-txt)
  (interactive)
  (write-region (format-time-string "%Y%m%d\n") nil last-package-refresh-file)
  (message (concat mess-txt (format-time-string "%m/%d/%Y %H:%M:%S")))
  (package-refresh-contents))
(if (equal (format-time-string "%a")
           (if (not (string-equal system-type "windows-nt"))
               "Sun"
             "Mon"))
    (if (file-exists-p last-package-refresh-file)
        (progn
          (with-temp-buffer
            (insert-file-contents last-package-refresh-file)
            (setq we-last-package-refresh-date (buffer-string)))
          (if (equal we-last-package-refresh-date (format-time-string "%Y%m%d\n"))
              (message "1: Found same date")
            (we/package-refresh-contents "2: Different date, refresh package contents on ")))
      (we/package-refresh-contents "3: No Date file, refresh package contents on ")))


;; Diminish; require this first so it can be used everywhere as I used to use it
;; as part of use-package
(require 'diminish)

;; Load defaults
(load "we-defaults")

;; Load spaceline
(load "we-spaceline")

;; Set font
(load "we-font")

;; Set theme
(load "we-theme")

;; Load dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-show-shortcuts nil)
(if (string-equal system-type "windows-nt")
    (setq dashboard-startup-banner "~/.emacs.d/img/dont-tread-on-emacs-150.png")
  (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png"))
(setq dashboard-banner-logo-title "Get Hacking!")
(setq dashboard-items '((recents  . 15)
                        ))
;; ref: https://github.com/emacs-dashboard/emacs-dashboard
(setq dashboard-set-init-info 1)
(setq dashboard-footer-icon (all-the-icons-octicon "zap"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))
(setq dashboard-set-footer 1)

;; Load General.el, the Cadillac of key-binders
(load "we-general")

;; Load utility packages.
(load "we-utilities")

;; Load auto-completion; this is completion of words, code, etc.
(load "we-auto-complete")

;; Load completion; this is command completion and/or command narrowing.
(load "we-completion")

;; Load Dired
(load "we-directory")

;; Load Personal functions; some of these are used elsewhere, like hydras.
(load "we-personal")

;; Load Hydras
(load "we-hydras")

;; Load Evil Mode packages
(load "we-evil")

;; Load org-mode config
(load "we-org")

;; Load Navigation packages
(load "we-navigation")

;; Load Editing packages.
(load "we-editing")

;; Load the useful functions, personal, but not important.
(load "we-useful")


;; THIS NEEDS TO COME LAST!!!!
;; There are a few minor modes which are not getting diminished, so do that here.
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)
(diminish 'subword-mode)

;; Set defaults for work/home: default directory & files to open.
(if (string-equal system-type "windows-nt")
    (progn
      (find-file "c:/_work/org/todo.org")
      (find-file "c:/_work/org/acts.org")
      (find-file "c:/_work/org/jira.org")
      ;; 2020-02-28: add x12-mode autoload plus file extensions
      (progn
        (autoload 'x12-mode "x12-mode" "" t)
        ;; Add more file extensions as required
        (add-to-list 'auto-mode-alist '("\\.x12\\'" . x12-mode)))))