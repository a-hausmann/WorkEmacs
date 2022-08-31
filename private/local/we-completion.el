;; File:       we-completion.el  --- -*- lexical-binding: t -*-
;; Date:       2022-08-25
;; Purpose:    This file configures all the completion framework.
;;

;; Ref: https://github.com/minad/vertico
;; Also, https://config.daviwil.com/emacs and find "vertico"
;; David's custom function to backward kill in minibuffer
(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

;; Enable vertico
(require 'vertico)
(vertico-mode 1)
(customize-set-variable 'vertico-cycle t)
(define-key vertico-map (kbd "C-n") 'vertico-next)
(define-key vertico-map (kbd "C-p") 'vertico-previous)
(define-key vertico-map (kbd "C-g") 'vertico-exit)
(define-key minibuffer-local-map (kbd "<C-backspace>") 'dw/minibuffer-backward-kill)

(message "Completed Vertico Config")

; (use-package vertico
;   :init
;   (vertico-mode)
;   :custom
;   (vertico-cycle t)
;   :custom-face
;   (vertico-current ((t (:background "#3a3f5a"))))
;   :bind (:map vertico-map
;          ("C-n" . vertico-next)
;          ("C-p" . vertico-previous)
;          ("C-g" . vertico-exit)
;          :map minibuffer-local-map
;          ("<C-backspace>" . dw/minibuffer-backward-kill)))

;; 2022-08-04: Changes in vertico invalidated the below. Documentation for Projectile
;; indicates it will automatically use the default completion system, in my case, Vertico.
;; Testing showed that I don't need to set this variable at all.
;; (setq projectile-completion-system 'vertico)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))
(require 'orderless)

(message "Completed Orderless Config")

; (use-package orderless
;   :init
;   (setq completion-styles '(orderless)
;         completion-category-defaults nil
;         completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode 1)
(require 'savehist)

(message "Completed Savehist Config")

; (use-package savehist
;   :init
;   (savehist-mode))

;; Additional packages for completions and enrichments
;; Consult provides functionality similary to Counsel.
;; Ref: https://github.com/minad/consult
;; Also: https://config.daviwil.com/emacs and search for "Consult Commands"
;; Copy of David's code for projectile functions.
(defun we/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Configure Consult
(setq register-preview-delay 0
      register-preview-function #'consult-register-format)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'consult)
(diminish 'consult)

(setq consult-project-root-function #'we/get-project-root
      completion-in-region-function #'consult-completion-in-region)
(advice-add #'register-preview :override #'consult-register-window)

(general-define-key
  "C-s"  'isearch-forward                 ;; Still useful, consult has no better solution.
  "C-c C-r"  'isearch-backward            ;; Still useful, consult has no better solution.
  "C-c C-s"  'consult-isearch-forward     ;; works in mini-buffer ONLY!
  "C-S-s"  'consult-line
  "C-M-l"  'consult-imenu
  "C-x b"  'consult-buffer                ;; orig. switch-to-buffer
  "C-x 4 b"  'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
  "C-x 5 b"  'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
  "C-M-'"  'consult-register-store        ;; dwim register: store, append, prepend, optionally delete (prefix arg
  "M-'"  'consult-register-load           ;; dwim register: insert, jump, or restore (window config
  "C-M-#"  'consult-register
  "M-y"  'consult-yank-pop                ;; orig. yank-pop
  "C-c r"  'consult-recent-file
  "C-c g"  'consult-goto-line             ;; goto specified line
  "C-c m"  'consult-mark                  ;; jump to marker in the mark-ring
  "M-s f"  'consult-find
  "M-s g"  'consult-grep
  "M-s G"  'consult-git-grep
  "M-s l"  'consult-line                  ;; required by consult-line to detect isearch 
)

;; TODO: look at the below.
(setq consult-preview-key 'any
      consult-narrow-key "<")
(consult-customize
 consult-git-grep consult-grep
 consult-bookmark consult-recent-file
 consult--source-recent-file consult--source-bookmark
 :preview-key (kbd "M-."))

(message "Completed Consult Config")

;; Ref: https://github.com/gagbo/consult-lsp
(with-eval-after-load 'lsp-mode
  (require 'consult-lsp)
  (diminish 'consult-lsp)
  (define-key lsp-mode [remap xref-find-apropos] 'consult-lsp-symbols))

(message "Completed Consult-lsp Config")

; (use-package consult-lsp
;   :ensure t
;   :after lsp-mode
;   ;; :config
;   ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
;   ;; By all that's holy (https://github.com/jwiegley/use-package#key-binding) this should work but has not.
;   ;; :bind (:map lsp-mode-map
;   ;; However, merely binding globally instead of to lsp-mode-map does work, so will not need to put in config.
;   :bind ([remap xref-find-apropos] . consult-lsp-symbols))

;; Marginalia provides similar functionality as ivy-rich--which we LOVE!
;; Ref: https://config.daviwil.com/emacs search for Marginalia

(with-eval-after-load 'vertico
  (require 'marginalia)
  (diminish 'marginalia)
  (marginalia-mode 1)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

(message "Completed Marginalia Config")

; (use-package marginalia
;   :after vertico
;   :custom
;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;   :init
;   (marginalia-mode))


;; Embark is hard to describe, but provides ways to ACT upon completion items.
;; Ref: https://config.daviwil.com/emacs search for Embark

(require 'embark)
(diminish 'embark)
(global-set-key (kbd "C-S-a") 'embark-act)
(define-key minibuffer-local-map (kbd "C-d") 'embark-act)
(setq embark-action-indicator
    (lambda (map _target)
      (which-key--show-keymap "Embark" map nil nil 'no-paging)
      #'which-key--hide-popup-ignore-command)
    embark-become-indicator embark-action-indicator)

(message "Completed Embark Config")

; (use-package embark
;   :ensure t
;   :bind (("C-S-a" . embark-act)
;          :map minibuffer-local-map
;          ("C-d" . embark-act))
;   :config
;   ;; Show Embark actions via which-key, this from David 
;   ;; (setq embark-action-indicator
;   ;;       (lambda (map)
;   ;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
;   ;;         #'which-key--hide-popup-ignore-command)
;   ;;       embark-become-indicator embark-action-indicator)
; ;; Ref: https://github.com/oantolin/embark/ and search for "which-key"  
;   (setq embark-action-indicator
;       (lambda (map _target)
;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
;         #'which-key--hide-popup-ignore-command)
;       embark-become-indicator embark-action-indicator)
; )


;; Ref: https://github.com/minad/affe
;; Affe is an asynchronous fuzzy finder for emacs... Look into this.
;; As of today (2021-05-22) it is not yet available on ELPA or MELPA. However....
;; NOTE: THIS PROGRAM DOES REQUIRED EITHER FIND OR RIPGREP, SO USELESS TO ME ON WINDOWS!!!!!!

