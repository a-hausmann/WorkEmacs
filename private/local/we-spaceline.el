;; File:       we-spaceline.el
;; Date:       2022-08-25
;; Purpose:    This is a "require" version of "aeh-spaceline.el"
;; Reference:  https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#12-mode-line
;;

(defun we/delight-powerline-major-mode (original-function &rest arguments)
      (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))
(defun we/delight-powerline-minor-mode (original-function &rest arguments)
      (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))

(require 'delight)
(advice-add 'powerline-major-mode :around #'we/delight-powerline-major-mode)
(advice-add 'powerline-minor-mode :around #'we/delight-powerline-minor-mode)

(custom-set-faces
 '(spaceline-modified ((t (:background "#C11B17" :foreground "#000000"   ; chili pepper red
                                       :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "#00FF00" :foreground "#000000" ; lime green
                                         :inherit (quote mode-line)))))
 '(spaceline-read-only ((t (:background "#0000FF" :foreground "#FFFFFF"  ; blue
                     :inherit (quote mode-line))))))

; Require Spaceline
(require 'spaceline)
(spaceline-define-segment we/buffer-status
  "Buffer status: read-only, modified"
  (cond (buffer-read-only (propertize "RO" 'face 'spaceline-read-only))
        ((buffer-modified-p) (propertize "**" 'face 'spaceline-modified))
        ;; (t "--")))
        (t (propertize "--" 'face 'spaceline-unmodified))))

;; This is again one of Mathieu's customizations, but without the custom package function call. I should be able to use it.
(spaceline-define-segment we/version-control
  "Show the current version control branch."
  (when vc-mode
    (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))

; Require Spaceline-config
(require 'spaceline-config)
(setq-default
 mode-line-format '("%e" (:eval (spaceline-ml-main)))
 ;; powerline-default-separator 'utf-8
 powerline-default-separator 'arrow
 powerline-height 20
 spaceline-highlight-face-func 'spaceline-highlight-face-modified ;; OK, but I'd like to change foreground color.
 spaceline-flycheck-bullet "❖ %s"
 spaceline-window-numbers-unicode t
 spaceline-separator-dir-left '(left . left)
 spaceline-separator-dir-right '(right . right))

;; Build the mode-lines
(spaceline-compile
  ;; `main
  ;; Left hand side (lhs) definition
  '(
    (we/buffer-status)
    ((remote-host buffer-id) :face default-face :separator "|")
    ;; (buffer-id :face default-face)
    (evil-state)
    ;; (remote-host buffer-id)
    (process :when active))
  ;; Right hand side (rhs) definition
  '(
    (line-column :face highlight-face)
    (selection-info :face highlight-face)
    (major-mode :face highlight-face :separator " | ")
    (minor-modes)
    ;; ((flycheck-error flycheck-warning flycheck-info))
    ;; (which-function-mode)
    (version-control :when active)
    (buffer-encoding)
    (buffer-position)
    (buffer-size)
    (global :when active)
    (we/selection-info)
    )
  )

(spaceline-toggle-hud-on)
(powerline-reset)
