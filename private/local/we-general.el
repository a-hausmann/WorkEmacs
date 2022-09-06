;; File name:     ws-general.el
;; Last modified: Thu Sep 01, 2022 13:42:40
;; Author:        Arnold Hausmann

;;; Require which-key here as this is where we're going to hit up much of the key definitions.
;; Ref: https://github.com/justbur/emacs-which-key
(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)
(setq which-key-idle-delay 1)


;;; Require general.el, the masterpiece of generic key-definers.
;; Ref: https://github.com/noctuid/general.el 
(require 'general)
(general-evil-setup)

;; Prefix keybindings
(general-create-definer we-leader-def
  :prefix "SPC")
(general-create-definer we-local-leader-def
  :prefix "C-;")
;; Global keybindings
(we-leader-def
  :keymaps '(normal visual emacs)
  ";" '(frog-jump-buffer :which-key "Frog jump buffer")
  "TAB" '(we/switch-to-previous-buffer :which-key "prev-buffer")
  "b" '(we/hydra-buffers/body :which-key "buffers")
  "c" '(we/hydra-consult/body :which-key "consult")
  "f" '(we/hydra-files/body :which-key "files")
  "m" '(we/hydra-modes/body :which-key "modes")
  "M" '(we/hydra-magit/body :which-key "Magit")
  "r" '(we/hydra-rectangle/body :which-key "rectangle")
  "t" '(we/hydra-toggles/body :which-key "toggles")
  "y" '(we/hydra-yasnippet/body :which-key "snippets")
  "w" '(we/hydra-windows/body :which-key "windows")
)
(we-local-leader-def
  :keymaps 'insert
  ";" '(frog-jump-buffer :which-key "Frog jump buffer")
  "TAB" '(we/switch-to-previous-buffer :which-key "prev-buffer")
  "d" '(we/hydra-insert-date-menu/body :which-key "dates")
  "i" '(we/hydra-insert-stuff-menu/body :which-key "insert stuff")
)

;; As here we want to use dwim functions instead of the default upcase/downcase/capitalize-word,
;; we first need to undefine these keybindings.
(general-unbind
  "M-c"
  "M-l"
  "M-u")
(general-def
 "M-c u" 'upcase-dwim
 "M-c l" 'downcase-dwim
 "M-c c" 'capitalize-dwim)

