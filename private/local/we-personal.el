;; File name:     we-personal.el
;; Last modified: Wed Nov 16, 2022 14:33:14
;; Author:        Arnold Hausmann

;; Function and hook to set the last modified date in any buffer/file
;; which contains the "Last modified: " string or "#+date: " string
;; in an org buffer.  100% of the code is here, so no need for a
;; separate file.
;; 2022-11-16: So, SOMETIMES I will create a txt file and set to mode: org,
;; and since it lacks Org modifiers, I want to also look for "Last modified".
;; If "Last modified" insert usual timestamp, if "#+date", use org-time-stamp.
(defun we/set-last-modified-ts ()
  "Set new timestamp for \"Last modified: \" tag, or if in org-mode,
the \"#+date: \" tag.  Function searches for string from point-min forward;
when found, it deletes from point (at end of search string) to point-at-eol,
then inserts current time in specified format. "
  (interactive)
  (if (equal major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (cond ((search-forward "#+date: " nil t)
               ;; It appears I do NOT need a lambda here, just execute two functions on when()
               (delete-region (point) (point-at-eol))
               (let ((current-prefix-arg '(16)))
                 (call-interactively 'org-time-stamp)))
              ((search-forward "Last modified: " nil t)
               (delete-region (point) (point-at-eol))
               (insert (format-time-string "%a %b %d, %Y %-H:%M:%S")))))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "Last modified: " nil t)
        ;; It appears I do NOT need a lambda here, just execute two functions on when()
        (delete-region (point) (point-at-eol))
        (insert (format-time-string "%a %b %d, %Y %-H:%M:%S"))))))
;; Set hook to execute function before writing.
(add-hook 'before-save-hook
          (lambda () (we/set-last-modified-ts)))


;; This stuff is stolen code from Magnar Sveen, from his What The Emacs blog (http://whattheemacsd.com).
;; It is pretty self explanatory. It could be pretty helpful (knowing how OCD I am about
;; not using tabs or having trailing spaces.)

(defun we/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun we/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (we/cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))


;; Hang onto this just in case my normal and insert mode leader keys become useless.
(defun we/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(general-def "C-x p" 'we/switch-to-previous-buffer)



;; Delete current buffer file. This is one of Bailey Ling's functions from "config-util.el"
(defun aeh/delete-current-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; Rename current buffer file. This is one of Bailey Ling's functions from "config-util.el."
(defun aeh/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Copy file name to clipboard. This is one of Bailey Ling's functions from "config-util.el."
(defun we/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; Convert buffer to DOS format. This is one of Bailey Ling's functions from "config-util.el."
(defun we/set-buffer-to-dos-format ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


;; Convert buffer to UNIX format. This is one of Bailey Ling's functions from "config-util.el."
(defun we/set-buffer-to-unix-format ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))


(defun we/insert-default-directory ()
  "Insert the value of default-directory at point"
  (interactive)
  (insert default-directory))

(defun we/insert-current-file-name ()
  "Insert the value of current file name at point"
  (interactive)
  (insert buffer-file-name))

(defun we/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(general-def "C-x 2" 'we/split-and-follow-horizontally)

(defun we/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(general-def  "C-x 3" 'we/split-and-follow-vertically)

;; Killing buffers cleanly By default x k is bound to kill-buffer.
;; Instead, we want to kill the current buffer.
(defun we/kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(general-def "C-x k" 'we/kill-current-buffer)


;; Dwim narrowing the way I like.
(defun we/narrow-dwim ()
  "Toggle narrowing."
  (interactive)
  (cond ((region-active-p)
          ;; If region is highlighted, narrow to that
          (call-interactively #'narrow-to-region)
          (deactivate-mark t))
    ((buffer-narrowed-p)
      ;; Otherwise widen if narrowed
      (widen))
    ((derived-mode-p 'org-mode)
      (call-interactively #'org-narrow-to-subtree))
    (t
      (message "Do not know what to narrow to.")
      (call-interactively #'narrow-to-defun))))
(general-def "C-x n w" 'we/narrow-dwim)


;; Created to prettify MBD Rules for me or Mary.
(defun we/make-pretty (p-from p-thru)
  "Prettify Rule code by moving all and/or conjunctions to a new line"
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((change-count 0))
          (goto-char p-from)
          (while (re-search-forward "\\( and \\| or \\)" p-thru t )
            (setq change-count (+ change-count 1))
            (replace-match "
\\1" nil nil))
          (message (format "Made %d changes." change-count)))))))

(defun we/prettify-rule-dwim ()
  "The dwim will allow for prettifying by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (we/make-pretty (region-beginning) (region-end)))
    (t (we/make-pretty (point-min) (point-max)))))


;; This dwim will delete carriage returns.
(defun we/delete-carriage-return-dwim ()
  "The dwim will delete carriage return by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (we/strip-ctl-m (region-beginning) (region-end)))
    (t (we/strip-ctl-m (point-min) (point-max)))))


;; Strip the carriage returns from a UNIX file copied from DOS/Windoze
(defun we/strip-ctl-m (p-from p-thru)
  "Replace carriage returns (^M) with nil"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (re-search-forward (concat (char-to-string 13) "$") p-thru t)
            (setq remove-count (+ remove-count 1))
            (replace-match "" nil nil))
          (message (format "%d ^M removed from buffer." remove-count)))))))

;; Reset text scale to normal
(defun we/text-scale-reset ()
  "Wrapper for \"(text-scale-increase 0)\""
  (interactive)
  (text-scale-increase 0))
(general-def
  "C-M--" 'text-scale-decrease
  "C-+" 'text-scale-increase
  "C-M-=" 'we/text-scale-reset)


;; Set previous word to upcase.
(defun we/upcase-last-word ()
  "Set previous word to upcase"
  (interactive)
  (upcase-word -1))
(general-def
  "C-C z u" 'we/upcase-last-word)


;; Set previous word to downcase.
(defun we/downcase-last-word ()
  "Set previous word to downcase"
  (interactive)
  (downcase-word -1))
(general-def
  "C-C z d" 'we/downcase-last-word)


;; Set previous word to capitalize.
(defun we/capitalize-last-word ()
  "Set previous word to capitalize"
  (interactive)
  (capitalize-word -1))
(general-def
  "C-C z c" 'we/capitalize-last-word
  "C-c z C" 'capitalize-word)


