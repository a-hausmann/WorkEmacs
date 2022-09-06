;; -*- lexical-binding: t -*-
;; File:          we-editing.el
;; Created:       2022-08-30
;; Last modified: Thu Sep 01, 2022 15:05:36
;; Purpose:       Load package which aid in editing.
;;

;; Configure Beacon
(require 'beacon)
(diminish 'beacon-mode)
(beacon-mode 1)


;; Configure column-enforce-mode
(require 'column-enforce-mode)
(diminish 'column-enforce-mode)
(add-hook 'prog-mode-hook 'column-enforce-mode)
(setq column-enforce-comments nil)


;; Configure Drag-stuff
(require 'drag-stuff)
(diminish 'drag-stuff-mode)
(drag-stuff-define-keys)
(general-def "M-<f3>" 'drag-stuff-mode)


;; 2022-08-30: Commenting out BOTH EditorConfig and ws-butler as getting warnings and don't
;; really use this package anyway.
;; Configure EditorConfig for Emacs.
;; (require 'editorconfig)
;; (diminish 'editorconfig)
;; (editorconfig-mode 1)
;; (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)


;; Configure WS-Butler, a package used to trim trailing whitespaces ONLY on changed lines.
;; (require 'ws-butler)
;; (diminish 'ws-butler)
;; (add-hook 'sql-mode-hook 'ws-butler-mode)
;; (add-hook 'sh-mode-hook 'ws-butler-mode)
;; (add-hook 'emacs-lisp-mode-hook 'ws-butler-mode)


;; Configure expand-region
(require 'expand-region)
(diminish 'expand-region)
(general-def "C-=" 'er/expand-region)


;; Configure Highlight-thing; highlights all occurances of the "thing" under point.
;; Generally have found only the "word" under point useful, and not ALL the time, so
;; provide toggle in "C-<f3>" and set to "word" for thing.
(require 'highlight-thing)
(diminish 'highlight-thing-mode)
(setq highlight-thing-what-thing 'word
      highlight-thing-case-sensitive-p nil)
(general-def "C-<f3>" 'highlight-thing-mode)


;; Configure Origami, the code-folding package.
(defun we/set-origami-fold-style-braces ()
  "Set origami fold-style to triple braces.

Additionally, display line numbers if not already doing so, enable origami,
and close all nodes.

This is designed to be used in a prog-mode-hook."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (message "Already displaying line numbers")
    (display-line-numbers-mode))
  (setq-local origami-fold-style 'triple-braces)
  (origami-mode)
  (origami-close-all-nodes (current-buffer)))

(defun we/origami-toggle-node ()
  (interactive)
  (save-excursion ;; leave point where it is
    (goto-char (point-at-eol))                         ;; then go to the end of line
    (origami-toggle-node (current-buffer) (point))))   ;; and try to fold

(require 'origami)
(diminish 'origami-mode)
(add-hook 'prog-mode-hook 'we/set-origami-fold-style-braces)

;; Ref: https://github.com/noctuid/general.el#basic-examples
;; Ref: https://github.com/noctuid/general.el#evil-examples
(general-def
 :states 'normal
 :keymaps '(prog-mode-map origami-mode-map)
 "TAB" 'we/origami-toggle-node
 "za" 'origami-forward-toggle-node
 "zR" 'origami-close-all-nodes
 "zM" 'origami-open-all-nodes
 "zr" 'origami-close-node-recursively
 "zm" 'origami-open-node-recursively
 "zo" 'origami-show-node
 "zc" 'origami-close-node
 "zj" 'origami-forward-fold
 "zk" 'origami-previous-fold)


;; Configure Rainbow-mode, useful for showing color of codes.
(require 'rainbow-mode)
(diminish 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'org-mode-hook 'rainbow-mode)


;; Configure Smartparens
(require 'smartparens)
(diminish 'smartparens-mode)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil) ; don't pair single quotes in elisp
(smartparens-global-mode 1)
