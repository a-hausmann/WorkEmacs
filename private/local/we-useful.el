;; -*- lexical-binding: t -*-
;; we-useful.el
;; File name:     we-useful.el
;; Last modified: Tue Aug 30, 2022 16:59:08
;; Author:        Arnold Hausmann
;; Why:           This is where I will keep useful code fragments/functions.

(defun we/new-untitled-buffer ()
  "Create new buffer named \"untitled\""
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (text-mode))
(global-set-key (kbd "C-c n") 'we/new-untitled-buffer)

(defun we/ff ()
  "Display positions at begin and end of a region."
  (interactive)
  (message "begin at %s; end at %s" (region-beginning) (region-end)))

(defalias 'ff 'we/ff)

(provide 'we/useful)
