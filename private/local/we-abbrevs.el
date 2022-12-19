;; -*- coding:utf-8; lexical-binding: t; -*-
;; File:          we-abbrevs.el
;; Created:       2022-12-07
;; Last modified: Thu Dec 08, 2022 12:15:13
;; Purpose:       Create abbreviation table(s).
;; Reference:     http://xahlee.info/emacs/emacs/emacs_abbrev_mode.html
;; Reference:     https://www.emacswiki.org/emacs/AbbrevMode
;;

(setq save-abbrevs nil)      ;; Do NOT save abbrevs when quiting emacs
(diminish 'abbrev-mode)

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; Days
    ("Mon" "Monday" )
    ("Tue" "Tuesday" )
    ("Wed" "Wednesday" )
    ("Fri" "Friday" )
    ("Sat" "Saturday" )
    ("Sun" "Sunday" )

    ;; Months, don't need all, just the longer ones.
    ("Jan" "January")
    ("Feb" "February")
    ("Aug" "August")
    ("Sep" "September")
    ("Oct" "October")
    ("Nov" "November")
    ("Dec" "December")

    ;; programing
    ("subdir" "subdirectory" )
    ("-*-" "-*- mode:  -*-")

    ;; common words, phrases
    ("enc" "Encounter")
    ("dl" "Data Loader")
    ("ARV" "Applied Rules Viewer")
    ("Lo" "Loyola")
    ))

(set-default 'abbrev-mode t)
