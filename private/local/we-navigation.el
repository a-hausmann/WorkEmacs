;; -*- lexical-binding: t -*-
;; File:          we-navigation.el
;; Created:       2022-08-29
;; Last modified: Thu Sep 01, 2022 15:18:31
;; Purpose:       Configure Emacs navigation packages. This includes searching,
;;                windows and jumping.
;;

;;; Ace-window configuration.
;; Have no idea why this was "needed".
;; (defun we/scroll-other-window()
;;   (interactive)
;;   (scroll-other-window 1))
;; (defun we/scroll-other-window-down ()
;;   (interactive)
;;   (scroll-other-window-down 1))

(require 'ace-window)
(diminish 'ace-window-mode)
(set-face-attribute
  'aw-leading-char-face nil
  :foreground "deep sky blue"
  :weight 'bold
  :height 3.0)
(set-face-attribute
  'aw-mode-line-face nil
  :inherit 'mode-line-buffer-id
  :foreground "lawn green")

(setq aw-dispatch-always nil)
(defvar
      aw-dispatch-alist
      '((?x aw-delete-window "Delete Window")
        (?m aw-swap-window "Swap Window")
        (?M aw-move-window "Move Window")
        (?c aw-copy-window "Copy Window")
        (?j aw-switch-buffer-in-window "Select Buffer")
        (?n aw-flip-window "Flip Window")
        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?v aw-split-window-vert "Split Vert Window")
        (?b aw-split-window-horz "Split Horz Window")
        (?o delete-other-windows "Delete Other Windows")
        ;; (?b balance-windows "Balance Windows")
        ;; (?u (lambda ()
        ;;       (progn
        ;;         (winner-undo)
        ;;         (setq this-command 'winner-undo))) "Winner Undo")
        ;; (?r winner-redo "Winner Redo")
        )
      "List of actions for `aw-dispatch-default'.")
(ace-window-display-mode t)
;; Cannot get this to work for now, what is NAME of ace window mode map???
;; (define-key ace-window-mode-map [remap other-window] 'ace-window)


;; Avy configuration
(require 'avy)
(diminish 'avy-mode)
(setq avy-timeout-seconds 1.0)
(general-def "C-x C-t" 'avy-goto-char-timer)


;; Frog-jump buffer configuration.
(require 'frog-jump-buffer)
(diminish 'frog-jump-buffer-mode)


;; Projectile configuration. Where to put Projectile? It DOES have something to do with navigating projects!
(require 'projectile)
(diminish 'projectile-mode)
(projectile-mode 1)
(general-def
 :keymaps 'projectile-mode-map
 "s-p" 'projectile-command-map
 "C-c p" 'projectile-command-map)

