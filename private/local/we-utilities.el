;; -*- lexical-binding: t -*-
;; File: we-utilities.el
;; Created:       2022-08-29 14:52:27
;; Last modified: Fri Dec 16, 2022 9:49:41
;; Purpose: Configure "utility" packages.

;; Yasnippet, first one up as I need that badly for new files.
(require 'yasnippet)
(diminish 'yas-global-mode)
(diminish 'yas-minor-mode)
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(yasnippet backquote-change) warning-suppress-types)
(setq yas-snippet-dirs '("~/.emacs.d/private/snippets"))
(setq yas-indent-line 'fixed)
(yas-global-mode 1)
(require 'yasnippet-snippets)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hook 'yas-minor-mode)


;; Configure amx, a newer alternative to smex.
(require 'amx)
(diminish 'amx-mode)
(amx-mode t)
(general-def
 "M-x" 'amx
 "M-X" 'amx-major-mode-commands
 "C-c C-c M-x" 'execute-extended-command)


;; Configure async
;; Ref: https://github.com/jwiegley/emacs-async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Configure command-log-mode
(require 'command-log-mode)
(diminish 'command-log-mode)
(general-def "C-c o" 'clm/toggle-command-log-buffer)


;; Configure Garbage Collector Magic Hack.
(require 'gcmh)
(diminish 'gcmh-mode)
(gcmh-mode 1)


;; Configure Helpful (help commands)
(require 'helpful)
(diminish 'helpful-mode)
(general-def
  [remap describe-function] 'helpful-callable
  [remap describe-command] 'helpful-command
  [remap describe-variable] 'helpful-variable
  [remap describe-key] 'helpful-key)


;; Configure Magit. The "require" automatically sets "C-x g" to magit-status, so
;; MUST redefine to nil first, then reassign "C-x g" as magit prefix.
(require 'magit)
(diminish 'magit)
(setq magit-push-always-verify nil
      git-commit-summary-max-length 50)
(general-def
  "C-x g" nil
  "C-x g g" 'magit-status
  "C-x g b" 'magit-blame
  "C-x g c" 'magit-branch-checkout
  "C-x g l" 'magit-log-buffer-file)

(require 'magit-gitflow)
(diminish 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;; Configure Try; this package allows one to "try" other packages without
;; really installing them.
(require 'try)


;; Configure undo-fu
;; (with-eval-after-load 'evil
(require 'undo-fu)
(require 'undo-fu-session)
(global-undo-fu-session-mode 1)
(general-def 'normal evil-normal-state-map
  "u" 'undo-fu-only-undo
  "C-r" 'undo-fu-only-redo)
(setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
(if (file-directory-p "~/.emacs.d/undo-fu-session")
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-fu-session")))
  (progn
    (dired-create-directory "~/.emacs.d/undo-fu-session")
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-fu-session")))))
  ;; )
