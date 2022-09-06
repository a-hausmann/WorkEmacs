;; File:          we-directory.el  --- -*- lexical-binding: t -*-
;; Created:       2022-08-26
;; Last modified: Thu Sep 01, 2022 14:20:42
;; Purpose:       Packages and settings for directory management.
;;


;;; Dired stuff documentation first!

;; Package dired-git-info:
;; Ref: http://xenodium.com/showhide-emacs-dired-details-in-style/
;; Repo: https://github.com/clemera/dired-git-info.
;; This works OK in Windows, but is very slow as it appears to send
;; a "git log" command string to Git for each file...probably best
;; to narrow the dired buffer before invoking this mode.

;; Dired-narrow: 
;; Ref: https://github.com/zamansky/using-emacs/blob/master/myinit.org#dired
;; Pragmaticemacs: http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/

;; Dired-subtree:
;; Ref: https://github.com/zamansky/using-emacs/blob/master/myinit.org#dired
;; 2019-06-29: Mike's config methodology for keybindings is not good. Better to use the :bind macro instead.
;; 2021-03-04: 'dired-listing-switches' doesn't seem to work for grouping directories first, so remove it as
;; that seemed to affect the date values--they were wrong.  Could be mix of dired sub-packages messing with
;; it, but leave that for another day.
;; 2021-03-05: Saw video showing 'dired-listing-switches' is a customizable variable, so MUST be set in ":custom".
;; However, seems to also need GNU Coreutils, so installed that, and added to %path%, but still doesn't seem to work.
;; The "G" does eliminate group info, and "D" is supposed to "generate output designed for Emacs dired mode" according
;; to the "ls" info-page, but that doesn't list "--group-directories-first", so that appears to be the end of it on Windows.
;; It seems the best thing to do is set variable to the string to execute based on Windows or not.

;;; First set up all-the-icons-dired and set the dired-string
(require 'all-the-icons-dired)
; (use-package all-the-icons-dired
;     :diminish)

(if (string-equal system-type "windows-nt")
    (setq my/dired-string "-alG")
    (setq my/dired-string "-alG --group-directories-first"))

(require 'dired)
(setq dired-listing-switches my/dired-string)
(add-hook 'dired-load-hook
          (lambda ()
            (interactive)
            (dired-collapse)))
(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (all-the-icons-dired-mode 1)
            (hl-line-mode 1)))
(general-def "C-c d" 'dired-jump)
(evil-set-initial-state 'dired-mode 'normal)  ;; Notes Evil loads first.
(general-def
 :states 'normal
 :keymaps 'dired-mode-map
 "(" 'dired-hide-details-mode
 "j" 'dired-next-line
 "k" 'dired-previous-line
 "h" 'dired-up-directory
 "H" 'dired-hide-dotfiles-mode
 "l" 'dired-find-alternate-file
 "o" 'dired-find-file-other-window
 "s" 'dired-sort-toggle-or-edit
 "v" 'dired-toggle-marks
 "m" 'dired-mark
 "u" 'dired-unmark
 "U" 'dired-unmark-all-marks
 "c" 'dired-create-directory
 "q" 'kill-this-buffer
 "gg" 'revert-buffer
 "M-s" 'avy-goto-char-timer
 "W" 'evil-forward-WORD-begin
 "B" 'evil-backward-WORD-begin
 "E" 'evil-forward-WORD-end
 "" 'dired-git-info-mode
 "n" 'dired-next-line
 "p" 'dired-previous-line)
(general-def
 :keymaps 'dired-mode-map
 "SPC" nil)
(message "Completed dired configuration")

(with-eval-after-load 'dired
  (require 'dired-single))
(message "Completed dired-single configuration")

(with-eval-after-load 'dired
  (require 'dired-collapse))
(message "Completed dired-collapse configuration")

(with-eval-after-load 'dired
  (require 'dired-git-info)
  (general-def
    :keymaps 'dired-mode-map
    ")" 'dired-git-info-mode))
(message "Completed dired-git-info configuration")

(with-eval-after-load 'dired
  (require 'dired-narrow)
  (general-def
    :keymaps 'dired-mode-map
    "C-c C-n" 'dired-narrow
    "C-c C-f" 'dired-narrow-fuzzy
    "C-x C-n" 'dired-narrow-regexp))
(message "Completed dired-narrow configuration")

(with-eval-after-load 'dired
  (require 'dired-subtree)
  (general-def
    :keymaps 'dired-mode-map
    "<tab>" 'dired-subtree-toggle
    "<backtab>" 'dired-subtree-cycle))
(message "Completed dired-subtree configuration")

(with-eval-after-load 'dired
  (require 'dired-hide-dotfiles))
(message "Completed dired-hide-dotfiles configuration")


;;; Treemacs configuration
;; In Windows, Python 3.10 installed as "python.exe"
(if (string-equal system-type "windows-nt")
    (setq python-string "python")
    (setq python-string "python3"))
(setq my/python-path (executable-find python-string))
(message (concat "Python executable is: " my/python-path))

(with-eval-after-load 'winum
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
(require 'treemacs)
(general-def
  "M-<f2>" 'treemacs
  "C-x t t" 'treemacs)
(general-def
  :keymaps 'treemacs-mode-map
  "M-0" 'treemacs-select-window
  "C-x t 1" 'treemacs-delete-other-window
  "C-x t B" 'treemacs-bookmark
  "C-x t C-t" 'treemacs-find-file
  "C-x t M-t" 'treemacs-find-tag)
(progn
  (setq treemacs-collapse-dirs              (if (executable-find python-string) 3 0)
        treemacs-deferred-git-apply-delay   0.5
        treemacs-display-in-side-window     t
        treemacs-file-event-delay           5000
        treemacs-file-follow-delay          0.2
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  (treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find python-string))))
    (`(t . t)
      (treemacs-git-mode 'extended))
    (`(t . _)
      (treemacs-git-mode 'simple))))

; (use-package treemacs
;     :ensure t
;     ;; :disabled
;     :defer t
;     :commands (treemacs)
;     :bind (("M-<f2>" . treemacs))
;     (:map global-map
;           ("M-0"       . treemacs-select-window)
;           ("C-x t 1"   . treemacs-delete-other-windows)
;           ("C-x t t"   . treemacs)
;           ("C-x t B"   . treemacs-bookmark)
;           ("C-x t C-t" . treemacs-find-file)
;           ("C-x t M-t" . treemacs-find-tag))
;     :diminish " U"
;     :init
;     (with-eval-after-load 'winum
;       (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;     :config
;     (progn
;       (setq treemacs-collapse-dirs              (if (executable-find python-string) 3 0)
;             treemacs-deferred-git-apply-delay   0.5
;             treemacs-display-in-side-window     t
;             treemacs-file-event-delay           5000
;             treemacs-file-follow-delay          0.2
;             treemacs-follow-after-init          t
;             treemacs-follow-recenter-distance   0.1
;             treemacs-goto-tag-strategy          'refetch-index
;             treemacs-indentation                2
;             treemacs-indentation-string         " "
;             treemacs-is-never-other-window      nil
;             treemacs-no-png-images              nil
;             treemacs-project-follow-cleanup     nil
;             treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;             treemacs-recenter-after-file-follow nil
;             treemacs-recenter-after-tag-follow  nil
;             treemacs-show-hidden-files          t
;             treemacs-silent-filewatch           nil
;             treemacs-silent-refresh             nil
;             treemacs-sorting                    'alphabetic-desc
;             treemacs-space-between-root-nodes   t
;             treemacs-tag-follow-cleanup         t
;             treemacs-tag-follow-delay           1.5
;             treemacs-width                      35)

;       ;; The default width and height of the icons is 22 pixels. If you are
;       ;; using a Hi-DPI display, uncomment this to double the icon size.
;       (treemacs-resize-icons 44)

;       (treemacs-follow-mode t)
;       (treemacs-filewatch-mode t)
;       (treemacs-fringe-indicator-mode t)
;       (pcase (cons (not (null (executable-find "git")))
;                    (not (null (executable-find python-string))))
;         (`(t . t)
;           (treemacs-git-mode 'extended))
;         (`(t . _)
;           (treemacs-git-mode 'simple))))
;     )

; 2022-08-26: Again, I do NOT KNOW how/why use-package now working, and would
; LOVE to convert to require, however I don't know how to handle the ":after"
; with multiple packages, and "with-eval-after-load" doesn't seem to handle that
; well, if at all.
; Leaving these as use-package for now.

(use-package treemacs-evil
    :after treemacs evil
    ;; :disabled
    :ensure t)

(use-package treemacs-projectile
    :after treemacs projectile
    ;; :disabled
    :ensure t)

;; 2019-10-16: added package
(use-package treemacs-icons-dired
    :after treemacs dired
    ;; :disabled
    :ensure t
    :config (treemacs-icons-dired-mode))

