;; -*- lexical-binding: t -*-
;; File:          we-evil.el
;; Created:       2022-08-26
;; Last modified: Tue Aug 30, 2022 16:12:17
;; Purpose:       Configure all Evil Mode packages.
;;

; IMPORTANT! This MUST be set before starting up evil-mode!
(setq evil-want-keybinding nil)

; Evil mode
(require 'evil)
(diminish 'evil-mode)
(evil-mode 1)

;; Do NOT have to use evil in every mode, so let's make a list where evil is not used.
(dolist (mode '(ag-mode
                flycheck-error-list-mode
                paradox-menu-mode
                git-rebase-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; Start in insert mode for small buffers
(dolist (mode '(org-mode sql-mode lisp-mode text-mode))
  (add-to-list 'evil-normal-state-modes mode))

(evil-add-hjkl-bindings eww-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "C-f")     'evil-scroll-down
  (kbd "C-b")     'evil-scroll-up
  ;; (kbd "C-w C-w") 'other-window
  (kbd "C-w C-w") 'ace-window)

(define-key evil-normal-state-map (kbd "q") nil)


(require 'evil-collection)
(diminish 'evil-collection-unimpaired-mode)
(evil-collection-init)


(require 'evil-commentary)
(diminish 'evil-commentary-mode)
(add-hook 'prog-mode-hook 'evil-commentary-mode)


(require 'evil-exchange)
(evil-exchange-cx-install)


(require 'evil-matchit)
(diminish 'evil-matchit-mode)
(global-evil-matchit-mode 1)


(require 'evil-surround)
(diminish 'evil-surround-mode)
(global-evil-surround-mode)

