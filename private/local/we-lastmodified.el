;; File name:     we-lastmodified.el
;; Last modified: Tue Aug 30, 2022 16:17:53
;; Author:        Arnold Hausmann

;; Set string to search for: aeh/timestamp-string
(if (eq major-mode 'org-mode)
    (progn
      (setq aeh/timestamp-string "^#\\\\+DATE:[ \t]+")
      (setq aeh/line-limit 20)
      )
  (progn
    (setq aeh/timestamp-string ".*Last modified:[ \t]+")
    (setq aeh/line-limit 8)
    )
  )

;; Use constant for English Day Month Date, Year and time.
(defconst aeh/day-time-format "%a %b %d, %Y %-H:%M:%S" "English Date Time as: Day Mon Date, Year HH24:MI:SS")

(defun aeh/timestamp ()
  "Update the file's time stamp string in the buffer. This is TARGETED at files
in org-mode with string \"^#+DATE:\" (in lines 1 - 20) or for a file in any
other mode with string \".*Last modified:\" in the first 8 lines of the file.
Both strings are ASSUMED to start in the first or first couple columns in a
line, and are ASSUMED to have nothing but a timestamp after the string.
File timestamps will formatted as: \"Tue Jan 08, 2019 15:25:57\". Files which
do not have these strings will not have a timestamp inserted.
This function is generally executed by by a before-save-hook.
"
  (interactive)
  )
