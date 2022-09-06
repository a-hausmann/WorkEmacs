;; -*- lexical-binding: t -*-
;; File:          we-lsp-etc.el
;; Created:       2022-09-01
;; Last modified: Mon Sep 05, 2022 10:13:39
;; Purpose:       Configure LSP, Flycheck, jedi, etc. for Python and/or other language support.
;;

;; Configure Flycheck
;; Ref: https://emacs.stackexchange.com/questions/39241/flycheck-on-windows-python-pycompile-output-contained-no-errors
(defun we/flycheck-parse-output (output checker buffer)
  "Strip carriage return characters from flycheck output"
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))
;; Ref: https://www.flycheck.org/en/latest/languages.html#python
;; "Flycheck checks Python with python-flake8 or python-pylint,
;; and falls back to python-pycompile if neither of those is available."

(require 'flycheck)
(diminish 'flycheck-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pylint-executable "python")
(when (string-equal system-type "windows-nt")
  (advice-add #'flycheck-parse-output :override #'we/flycheck-parse-output))


;; Configure LSP Mode
;; Ref: https://emacs-lsp.github.io/lsp-mode/page/installation/
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "C-c C-c l")
;; 2021-02-22: Ref: https://github.com/daviwil/emacs-from-scratch/blob/6d078217a41134cc667f969430d150c50d03f448/Emacs.org
;; 2021-03-02: Ref: https://emacs-lsp.github.io/lsp-mode/page/installation/
(setq lsp-keymap-prefix "C-l")
(require 'lsp-mode)
(diminish 'lsp-mode)
(add-hook 'python-mode-hook 'lsp-deferred)
(lsp-enable-which-key-integration t)
(setq lsp-headerline-breadcrumb-enable t
      lsp-headerline-breadcrumb-segments '(project file symbols)
      lsp-headerline-breadcrumb-icons-enable t)

(require 'lsp-ui)
(diminish 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-ui-doc-position 'right)

(require 'lsp-treemacs)

;;------------------------------------------------------------------------------
;; 2022-08-05: Trying to fix errors after new PC and Python installed in weird
;; directory "C:\Users\frst6889\AppData\Local\Programs\Python\Python310\".
;; Ref: https://stackoverflow.com/questions/37720869/emacs-how-do-i-set-flycheck-to-python-3
;; NOTE: I don't have executable python3.exe, just python.exe
;; NOTE: ALSO created DOS environment variable "PYTHONPATH" to installed path.
(setq flycheck-python-flake8-executable "python"
      flycheck-python-pycompile-executable "python"
      flycheck-python-pylint-executable "python")
