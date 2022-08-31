;;; .emacs.d/init.el -- the `user-init-file'

(setq package-enable-at-startup nil)
(require 'package)

;; 2022-08-25: as package-initialize no longer seems to work, manually add to
;; load-path
; (package-initialize)

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; 2021-02-22: ELPA is evidently dead cannot get GPG public signature anymore.
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; 2018-12-27: Added the line below, had to look up correct URL, ref: https://emacsredux.com/blog/2014/05/16/melpa-stable/
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(message "Before package-initialize")
(package-initialize)
(message "After package-initialize")

;; 2022-01-05: add package-quickstart
(setq package-quickstart t)

;; Install dependencies
(unless (and (package-installed-p 'delight)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'use-package t))
(message "After install dependencies")

;; 2021-02-21: Missed actually requiring use-package, looking at System Crafters videos.
;; Ref: https://github.com/daviwil/dotfiles/blob/master/Emacs.org
(require 'use-package)
;; Uncomment this to get a reading on packages that get loaded at startup
(setq use-package-verbose t)

(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 ;; 2019-06-29: add statistics
 use-package-compute-statistics t)

;; 2019-09-05: installed this to track startup. Ref: https://github.com/dholm/benchmark-init-el
; (use-package benchmark-init
;   :ensure t
;   :config
;   ;; To disable collection of benchmark data after init is done.
;   (add-hook 'after-init-hook 'benchmark-init/deactivate))
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(benchmark-init/activate)
(message "After benchmark-init/activate")

;; 2018-12-16: migrate code from dotemacs.org file to here as it deals with version controlled files.
;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)

;; 2020-08-26: Add following to prevent "cl is deprecated" messages.
;; Ref: https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

(use-package-report)
(message "Generated use-package-report in init.el")
(delete-other-windows)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; 2019-09-05: put startup elapsed time with GC counter to *Messages* buffer.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; 2021-02-21: add setup for showing backtrace on errors.
(setq debug-on-error t)

;; Ensure all is set to UTF-8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

; 2022-08-25: this now needs to go in init.el for Windows without use-package.
(if (string-equal system-type "windows-nt")
    (add-to-list 'load-path "c:/Users/frst6889/.emacs.d/private/local")
  (add-to-list 'load-path "~/.emacs.d/private/local"))

(load "we-requires")

(message "init.el completed.")

;;; .emacs.d/init.el ends here
