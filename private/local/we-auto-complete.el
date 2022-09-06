;; File:          we-auto-complete.el  --- -*- lexical-binding: t -*-
;; Created:       2022-08-25
;; Last modified: Thu Sep 01, 2022 16:14:27
;; Purpose:       A separate loader for Auto-Completions, starting with Company.
;;

;; --------------------------------------------------------------------------------
;; I think that Company is acting a bit better now, and on 10/30/2018, I added
;; some code from Oleh Krehel https://oremacs.com/2017/12/27/company-numbers/
;; to show numbers on the popup, and be able to use them to select text.
;; This works like a charm. Oleh's blog article is from December 2017, and he
;; states his git log shows he's been using this setup for three years without
;; any issues.  Grand!
;; --------------------------------------------------------------------------------
;; Basic setting
(with-eval-after-load 'company
  (setq company-show-numbers t)
  ;; Oleh's function:
  (defun ora-company-number ()
    "Forward to `company-complete-number'.

  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
          (company-complete-number (string-to-number k)))))

  ;; Add some bindings
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    ;; This line UNBINDS RET key from closing the popup
    (define-key map (kbd "<return>") nil)))

;; --------------------------------------------------------------------------------
;; Start Company configuration.
;; --------------------------------------------------------------------------------
(require 'company)
(diminish 'company-mode)
(setq company-idle-delay 1.5)  ; delay from stopping typing and beginning completion.
;; (global-set-key (kbd "C-M-.") 'company-complete-common)
(setq company-minimum-prefix-length 3)   ; three letters needed for completion
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-downcase nil)      ; return candidates AS IS.
(general-def
  :keymaps 'company-active-map
  "M-n" nil
  "M-p" nil
  "C-n" #'company-select-next
  "C-p" #'company-select-previous
  "C-M-." 'company-complete-common)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'shell-mode-hook 'company-mode)
(add-hook 'sql-mode-hook
          #'(lambda ()
              (setq-default company-minimum-prefix-length 4)
              (setq-default company-dabbrev-code-ignore-case t)
              (setq-default completion-ignore-case t)))

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(with-eval-after-load 'company
  (require 'company-shell))
(add-hook 'shell-mode-hook 'shell-mode-company-init)


;; Ref: ref: https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
(with-eval-after-load 'company
  (require 'company-statistics)
  (diminish 'company-statistics-mode)
  (company-statistics-mode 1))

(with-eval-after-load 'company
  (require 'company-web))

;; Not sure what I was thinking here and no notes on it.
;; (use-package company-try-hard
;;     :ensure t
;;     :after company
;;     :bind
;;     ;; Change from C-<tab> to "C-." and "C-M-."
;;     (("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)
;;      :map company-active-map
;;      ("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)))

(with-eval-after-load 'company
  (require 'company-quickhelp)
  (diminish 'company-quickhelp-mode)
  (company-quickhelp-mode))

;; 2020-07-08, adding company-jedi, which uses jedi-core, but made for Company users
;; (defun we/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (use-package company-jedi
;;     :ensure t
;;     :disabled
;;     :after company
;;     :hook (python-mode-map . we/python-mode-hook))

(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf)          ; completion-at-point-functions
        (company-abbrev company-dabbrev)))


;; End of we-auto-complete.el
