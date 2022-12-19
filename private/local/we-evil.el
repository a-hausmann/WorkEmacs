;; -*- lexical-binding: t -*-
;; File:          we-evil.el
;; Created:       2022-08-26
;; Last modified: Wed Nov 30, 2022 15:16:48
;; Purpose:       Configure all Evil Mode packages.
;;

; IMPORTANT! This MUST be set before starting up evil-mode!
(setq evil-want-keybinding nil)

;; Ref: https://github.com/emacsmirror/undo-fu#evil-mode
;; As this example of use-package with :init, it appears "evil-undo-system" should be set FIRST.
(setq evil-undo-system 'undo-fu)

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
  (kbd "C-w C-w") 'ace-window)

;; (define-key evil-normal-state-map (kbd "q") nil)
(general-def
  :keymaps 'evil-normal-state-map
  "q" nil)


(require 'evil-collection)
(evil-collection-init)
(diminish 'evil-collection-unimpaired-mode)


(require 'evil-commentary)
(diminish 'evil-commentary-mode)
(add-hook 'prog-mode-hook 'evil-commentary-mode)


(require 'evil-exchange)
(evil-exchange-cx-install)


(require 'evil-matchit)
(global-evil-matchit-mode 1)
(diminish 'evil-matchit-mode)


(require 'evil-surround)
(global-evil-surround-mode)
(diminish 'evil-surround-mode)

