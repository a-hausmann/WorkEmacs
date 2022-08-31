;; File:          aeh-defaults.el  --- -*- lexical-binding: t -*-
;; Created:       2022-08-17 10:48:13
;; Last modified: Mon Aug 29, 2022 10:38:14
;; Purpose:       Normal "default" configuration stuff.
;;

;;; Set username and email address.
(setq user-full-name "Arnold Hausmann")
(if (string-equal system-type "windows-nt")
    (setq user-mail-address "Arnold.Hausmann@trinity-health.org")
    (setq user-mail-address "ArnoldH@comcast.net"))

;;; Set Menu, Tool, and Scroll bars. At this time, I have decided I like the Menu bar.
;; (if (string-equal system-type "windows-nt")
;;     (menu-bar-mode 1)
;;   (menu-bar-mode -1))
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Unset 20 key bindings which duplicate another 10 (C-M-1 ... C-M-10)
;; Ref: http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; Will now use C-1...10 and M-1...10 however I see fit, they can now be reassigned.
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))


;; Ensure all is set to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;;; Better defaults, ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(setq-default
 inhibit-startup-message t                        ; Stop startup messages
 ring-bell-function 'ignore                       ; Stop annoying bell
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 garbage-collection-messages t                    ; set to non-nil to see GC messages.
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 2                                  ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 tab-width 4                                      ; Set width for tabs
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 display-line-numbers-type 'visual                ; Display relative+visible line#, works with folding.
 recentf-max-saved-items 100                      ; abo-abo sets to 600, but I'm cautious.
 kill-ring-max 100                                ; Larger kill-ring
 dired-dwim-target t                              ; Allow direct to dwim target of move, copy commands
 make-backup-files nil                            ; Disable backup files
 auto-save-default nil                            ; Disable auto-save funtionality
 )

;;; Things you cannot set with "setq"
(global-visual-line-mode 1)                       ; Really, REALLY like visual line mode.
(show-paren-mode 1)                               ; Always show matching parens
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(mouse-avoidance-mode 'animate)                   ; Avoid collision of mouse with point KEEP ON ANIMATE
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(global-set-key (kbd "C-M-y") 'clipboard-yank)    ; 2020-05-07: add mapping to yank from clipboard

;;; Hooks
;; ALWAYS display line numbers when in prog-mode!
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defun we/json-mode-hook ()
   (setq-local js-indent-level 2))
(add-hook 'json-mode-hook 'we/js2-mode-hook)

;;; Enable narrowing the region instead of function.
(put 'narrow-to-region 'disabled nil)

;;; While "hl-line" works well in GUI, it's not so hot in terminal, so use only in GUI.
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

;;; Pretty symbols: changes lambda to an actual symbol, plus some others; works only in GUI mode.
(when window-system
  (use-package pretty-mode
      :ensure t
      :config
      (global-pretty-mode t)))

;;; Set default browser for system.
;; 2022-08-18: Found that using the default Windows browser works VERY well,
;; so get rid of the rest of this crap.
(if (string-equal system-type "windows-nt")
    (setq browse-url-browser-function 'browse-url-default-windows-browser)
    (setq browse-url-browser-function 'browse-url-generic  ; set Linux default browswer
          browse-url-generic-program "brave"))


;;; Sane tabs-or-spaces
;; Ref: https://dougie.io/emacs/indentation/#using-tabs-or-spaces-in-different-files
;; Set the defaults, NO tabs
(setq-default indent-tabs-mode nil)

;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun we/disable-tabs ()
  "Custom function to disable tabs"
  (interactive)
  (setq indent-tabs-mode nil))
(defun we/enable-tabs  ()
  "Custom function to enable tabs"
  (interactive)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'we/disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'we/disable-tabs)
(add-hook 'sql-mode-hook 'we/disable-tabs)
