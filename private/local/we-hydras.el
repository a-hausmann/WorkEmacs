;; File name:     we-hydras.el
;; Created:       2018-11-06
;; Last modified: Fri Aug 26, 2022 15:57:25
;; Purpose:       This will contain my personal hydra definitions.  Much of this is 
;;                based on Bailey Ling's hydras, so some changing of names, and deleting
;;                stuff he has that I do not (I favor ivy over helm, he uses both.
;; 2022-08-26:    Copied we-hydras to we-hydras and converted names.

(require 'hydra)
(autoload 'hydra-default-pre "hydra")

;; we/hydra-insert-date-menu
(defconst we/date-simple "%m/%d/%Y" "Simple format: MM/DD/YYYY")
(defconst we/date-format "%Y-%m-%d" "Simple date as YYYY-MM-DD")
(defconst we/date-file "%Y%m%d" "Simple format: YYYYMMDD")
(defconst we/date-time-format "%Y-%m-%d %-H:%M:%S" "Simple Date with Time: YYYY-MM-DD HH24:MI:SS")
(defconst we/day-format "%a %b %d, %Y" "English date as: Day Mon Date, Year")
(defconst we/day-time-format "%a %b %d, %Y %-H:%M:%S" "English Date Time as: Day Mon Date, Year HH24:MI:SS")
(defconst we/full-day-format "%A, %B %d, %Y" "English date as: Day, Month Date, Year")
(defconst we/full-day-date-format "%A, %Y-%m-%d" "English date as: Day, YYYY-MM-DD")
(defconst we/full-day-time-format "%A, %B %d, %Y %-H:%M:%S %p" "English Date Time as: Day, Month Date, Year HH:MI:SS PM")

;; 2019-06-06: NEW! This is the place to add Inserts of whatever I dream up.
(defhydra we/hydra-insert-stuff-menu (:color blue)
  "This hydra inserts so-called random stuff."
  ("f" (insert (file-name-nondirectory (buffer-file-name))) "base filename")
  ("F" (insert (expand-file-name (buffer-file-name))) "full filename")
  ("p" (insert (file-name-directory (buffer-file-name))) "path")
  )

(defhydra we/hydra-insert-date-menu (:color blue)
  "
_q_: quit
_s_: MM/DD/YYYY
_d_: YYYY-MM-DD
_f_: YYYYMMDD
_t_: YYYY-MM-DD HH24:MI:SS
_D_: DD Mon Date, Year
_T_: DD Mon Date, Year HH24:MI:SS
_A_: Day, YYYY-MM-DD
_e_: Day, Month Day, Year
_E_: Day, Month Day, Year HH:MI:SS PM
 "
  ("q" nil)
  ("s" (insert (format-time-string we/date-simple)))
  ("d" (insert (format-time-string we/date-format)))
  ("f" (insert (format-time-string we/date-file)))
  ("t" (insert (format-time-string we/date-time-format)))
  ("D" (insert (format-time-string we/day-format)))
  ("T" (insert (format-time-string we/day-time-format)))
  ("A" (insert (format-time-string we/full-day-date-format)))
  ("e" (insert (format-time-string we/full-day-format)))
  ("E" (insert (format-time-string we/full-day-time-format))))

;; This is a straight copy from the github site: https://github.com/abo-abo/hydra/wiki/Basics
;; I've removed the mapping as I'll do that via evil-leader.
;; 2018-11-10: I like the idea I found here: https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/
;; This sets more "natural" keys "+", and "-" to zoom in/out, and "0" to reset
(defhydra we/hydra-zoom (:hint nil)
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset" :color blue)
  ("q" nil "quit"))

;; 2019-05-28: This new zoom uses latest version of Hydra, "20190525.817".
;; This brings some new capabilities, not all working yet. I would LOVE to
;; get the "posframe" working so the menus display in a floating child frame.
;; Changing General setup to "newzoom."
(defhydra we/hydra-newzoom (:exit nil)
  ("+" text-scale-increase "Zoom in")
  ("-" text-scale-decrease "Zoom out")
  ("0" (text-scale-set 0) "Reset" :color blue)
  ("q" nil "Quit"))


;; Rockin' the buffers menu!
(defhydra we/hydra-buffers (:hint nil :exit t)
  "
   buffers:   _b_ → buffers              _i_ → ibuffer                 _k_ → kill buffer
              _p_ → prev buffer          _e_ → erase buffer            _E_ → erase buffer (force)
              _n_ → new buffer           _A_ → save buffer AS file     _r_ → rename buffer
              _R_ → rename uniquely      _g_ → revert buffer           _G_ → GOTO buffer
              _M_ → delete ^M            _C_ → cleanup buffer
"
  ;; ("b" #'counsel-ibuffer)
  ("b" #'consult-buffer)
  ("i" #'ibuffer)
  ("k" #'kill-this-buffer)
  ("p" #'we/switch-to-previous-buffer)
  ("e" #'erase-buffer)
  ("E" (let ((inhibit-read-only t)) (erase-buffer)))
  ("n" (switch-to-buffer (generate-new-buffer "untitled")))
  ("A" #'write-file)
  ("r" #'rename-buffer)
  ("R" #'rename-uniquely)
  ("g" #'revert-buffer)
  ("G" #'we/hydra-buffer-goto/body)
  ("M" #'we/delete-carrage-returns)
  ("C" #'ms/cleanup-buffer))

;; Buffer GOTO submenu
(defhydra we/hydra-buffer-goto (:exit t)
  "Buffer GOTO"
  ("d" (switch-to-buffer "*dashboard*") "GOTO *dashboard")
  ("m" (switch-to-buffer "*Messages*") "GOTO *Messages*")
  ("s" (switch-to-buffer "*scratch*") "GOTO *scratch*")
  ("S" (switch-to-buffer (get-buffer-create "*scratch*")) "Create *scratch*"))

;; Hydra for file conversions
(defhydra we/hydra-files-convert (:hint nil :exit t)
  "
  convert to:    _d_ → dos       _u_ → unix
  "
  ("d" we/set-buffer-to-dos-format)
  ("u" we/set-buffer-to-unix-format))

;; Hydra for bookmarks
(defhydra we/hydra-bookmarks (:hint nil :exit t)
  "
  bookmarks:    _j_ → jump to bookmark _l_ → list bookmarks    _s_ → set bookmark
                _D_ → delete bookmark
  "
  ("j" #'bookmark-jump)
  ("l" #'bookmark-bmenu-list)
  ("s" #'bookmark-set)
  ("D" #'bookmark-delete))

;; Hydra for files
(defhydra we/hydra-files (:hint nil :exit t)
  "
  files:
  _f_ → find files      _D_ → delete    _y_ → copy filename   _E_ → edit as root
  _r_ → recent files    _R_ → rename    _c_ → copy file       _C_ → convert        _b_ → bookmarks
  "
  ;; _z_ → fzf
  ("D" we/delete-current-buffer-file)	;; OK
  ("R" we/rename-current-buffer-file)	;; OK
  ;; ("f" #'counsel-find-file)		;; OK
  ("f" #'find-file)		;; OK
  ;; ("r" #'counsel-recentf)		;; OK
  ("r" #'consult-recent-file)		;; OK
  ("y" we/copy-file-name-to-clipboard)	;; OK
  ("E" we/edit-file-as-root)		;; OK
  ("c" copy-file)			;; OK
  ("C" we/hydra-files-convert/body)	;; OK
  ("b" we/hydra-bookmarks/body)        ;; OK
  ;; ("z" #'counsel-fzf))                 ;; OK
)

;; Hydra for toggles
;; 2018-11-18: have installed "autopair" as pairing package; Bailey used to switch between smartparens and electric-pairs, but
;; I don't see that happening for me at this point, so am removing this toggle.
(defvar we/hydras/toggles/vdiff nil)
(defhydra we/hydra-toggles (:hint nil :exit t)
  "
   toggle:  _z_ → origami-mode        _c_ → column-enforce
            _a_ → aggressive indent   _s_ → flycheck       _r_ → read only      _t_ → truncate lines   _e_ → debug on error
            _f_ → auto-fill           _S_ → flyspell       _C_ → completion     _W_ → word wrap        _g_ → debug on quit
            _w_ → whitespace          _E_ → electric-pairs _l_ → linum-relative _b_ → page break       _d_ → ediff/vdiff
            _h_ → highlight-thing     _D_ → drag-stuff
"
  ("z" origami-mode)
  ("c" column-enforce-mode)
  ("a" aggressive-indent-mode)
  ("b" page-break-lines-mode)
  ("C" company-mode)
  ("t" toggle-truncate-lines)
  ("e" toggle-debug-on-error)
  ("E" electric-pair-mode)
  ("h" highlight-thing-mode)
  ("l" display-line-numbers-mode)       ; display type set to "visual" in "Better-defaults"
  ("g" toggle-debug-on-quit)
  ("s" flycheck-mode)
  ("S" flyspell-mode)
  ("w" whitespace-mode)
  ("W" toggle-word-wrap)
  ("r" read-only-mode)
  ("f" auto-fill-mode)
  ("D" drag-stuff-mode)
  ;; Needs work on the "/pairs/toggle" code, copied from Bailey Ling's code base.
  ;; This switches between smartparens and electric-pairs; at this point, I'm not sure what is installed.
  ;; ("p" /pairs/toggle)
  ("d" (progn
         (if we/hydras/toggles/vdiff
	         (progn
	           (/bindings/vdiff/turn-off)
	           (message "using ediff"))
           (/vcs/setup-vdiff)
           (/bindings/vdiff/turn-on)
           (message "using vdiff"))
         (setq we/hydras/toggles/vdiff (not we/hydras/toggles/vdiff)))))


;; Hydra for rectangles
(defhydra we/hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :hint nil
                                         :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
  _h_   _l_   _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \- ;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _q_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil))      ;; ok

(defhydra we/hydra-org (:color red :hint nil)
"
Navigation^
---------------------------------------------------------
_q_uit
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
("q" nil :exit t)
("j" outline-next-visible-heading)
("k" outline-previous-visible-heading)
("h" org-forward-heading-same-level)
("l" org-backward-heading-same-level)
("u" outline-up-heading)
("g" org-goto :exit t))

;; 2018-11-19: create yasnippet hydra, set blue overall to quit upon command
(defhydra we/hydra-yasnippet (:color blue :hint nil)
  "
Snippets^
---------------------------------------------------------
_q_ uit
_i_ insert snippet
_n_ new snippet
_l_ load directory
_r_ reload all
_v_ visit snippet
_d_ describe table
"
  ("q" nil :exit t)
  ("i" yas-insert-snippet)
  ("n" yas-new-snippet)
  ("l" yas-load-directory)
  ("r" yas-reload-all)
  ("v" yas-visit-snippet-file)
  ("d" yas-describe-tables))


;; 2021-05-25: Add hydra for Consult commands. copy from we/hydra-counsel.
(defhydra we/hydra-consult (:color blue :hint nil)
  "
Consult^
---------------------------------------------------------
_q_ uit
_a_ apropos
_b_ bookmarks
_i_ imenu
_I_ project imenu
_m_ mark ring
_M_ global mark ring
_r_ recent files
_o_ outline headings
_O_ multi occur
_T_ load theme
_c_ mode command
_C_ minor mode menu
"
  ("q" nil :exit t)
  ("a" consult-apropos)
  ("b" consult-bookmark)
  ("i" consult-imenu)
  ("I" consult-project-imenu)
  ("m" consult-mark)
  ("M" consult-global-mark)
  ("r" consult-recent-file)
  ("o" consult-outline)
  ("O" consult-multi-occur)
  ("T" consult-theme)
  ("c" consult-mode-command)
  ("C" consult-minor-mode-menu)
  )



;; 2018-12-31: Add hydra for Counsel commands
(defhydra we/hydra-counsel (:color blue :hint nil)
  "
Counsel^
---------------------------------------------------------
_q_ uit
_a_ apropos
_b_ bookmarks
_B_ describe bindings
_f_ describe functions
_v_ describe variables
_c_ describe faces
_C_ list faces
_l_ find library
_L_ load library
_m_ imenu
_M_ mark ring
_r_ recent files
_T_ load theme
"
  ("q" nil :exit t)
  ("a" counsel-apropos)
  ("b" counsel-bookmark)
  ("B" counsel-descbinds)
  ("f" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("c" counsel-describe-face)
  ("C" counsel-faces)
  ("l" counsel-find-library)
  ("L" counsel-load-library)
  ("m" counsel-imenu)
  ("M" counsel-mark-ring)
  ("r" counsel-recentf)
  ("T" counsel-load-theme))

;; 2019-06-19: Add "Window" commands (check for frame commands)
;; Ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra
(defhydra we/hydra-windows (:color pink)
  "
^
^Windows^           ^Window^            ^Zoom^
^-------^-----------^------^------------^----^--------------
_q_ quit            _b_ balance         _-_ out
^^                  _i_ heighten        _+_ in
^^                  _j_ narrow          _=_ reset
^^                  _k_ lower           ^^
^^                  _l_ widen           ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))

;; 2019-06-20: Add Magit commands
;; Ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra
(defhydra we/hydra-magit (:color blue)
  "
^
^Magit^             ^Do^
^-----^-------------^--^----------------
_q_ quit            _b_ blame
^^                  _c_ clone
^^                  _i_ init
^^                  _s_ status
^^                  ^^
"
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status))

;; 2019-06-20: Add mode menu
(defhydra we/hydra-modes (:exit t)
  "Modes"
  ("o" #'orgtbl-mode "Org Table mode")
  ("p" #'prog-mode "Prog mode")
  ("s" #'sql-mode "SQL mode")
  ("t" #'text-mode "Text mode"))



(provide 'we-hydras)
