;; -*- lexical-binding: t -*-
;; File: we-org.el
;; Last modified: Tue Oct 11, 2022 8:56:40
;; Purpose: Configure org-mode

(setq org-startup-folded t)
(setq inhibit-compacting-font-caches t)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-face-name (quote org-bullet-face))
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; 2019-12-16: add to template-alist.
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; 2019-06-08: After loading ONE of MANY themes, the "fontify-natively" non-nil started
;; throwing code between source markers into horrid light colors regardless of theme used.
;; Setting the variable to nil gets rid of that tendency.
(setq org-src-fontify-natively nil)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)                   ; Allows for "C-c '" to narrow to code being edited.

;; The following lines are always needed. Choose your own keys.
(general-def
"C-c l" 'org-store-link
"C-c a" 'org-agenda
"C-c c" 'org-capture
"C-c b" 'org-switchb)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "ONGOING" "|" "DONE(d@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("ONGOING" :forground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
;; More from http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;normal todo list - not sure I want to ignore deadlines/schedules
;; (setq org-agenda-todo-ignore-deadlines (quote all))
;; (setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))

