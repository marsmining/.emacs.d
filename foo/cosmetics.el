(eval-after-load 'whitespace
  '(diminish 'whitespace-mode))
(eval-after-load 'paredit
  '(diminish 'paredit-mode))
(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load 'eldoc
  '(diminish 'eldoc-mode))
(eval-after-load 'diminish ; need to delay till after packages are all loaded
  '(diminish 'auto-fill-function))

;; lose the stupid pipe chars on the split-screen bar
(set-face-foreground 'vertical-border "white")
(set-face-background 'vertical-border "white")

(defun zb ()
  (interactive)
  (load-theme 'zenburn)
  (set-face-background 'vertical-border "black")
  (set-face-foreground 'vertical-border "black")
  (require 'hl-line)
  (set-face-background 'hl-line "gray17")
  ;; (set-face-foreground 'eshell-prompt "turquoise")
  )

(defun tw ()
  (interactive)
  (load-theme 'twilight)
  (set-face-background 'vertical-border "black")
  (set-face-foreground 'vertical-border "black")
  ;; (set-face-foreground 'font-lock-builtin-face "#5D92A5")
  (set-face-foreground 'font-lock-builtin-face "#a7a7a7")
  (set-background-color "#000")
  (set-face-foreground 'powerline-active1 "#fff")
  (require 'hl-line)
  ;; (set-face-foreground 'eshell-prompt "turquoise1")
  (set-face-background 'hl-line "#111"))

(defun mk ()
  (interactive)
  (load-theme 'monokai)
  (set-face-background 'vertical-border "black")
  (set-face-foreground 'vertical-border "black")
  (require 'hl-line)
  ;; (set-face-foreground 'eshell-prompt "turquoise1")
  (set-face-background 'hl-line "black"))

(defun bb ()
  "Black for use with glasstty in -nw"
  (interactive)
  (set-face-background 'vertical-border "bright green")
  (set-face-foreground 'vertical-border "bright green")
  (set-face-background 'hl-line "black"))

(eval-after-load 'hl-line
  '(set-face-background 'hl-line "darkseagreen2"))

;; TODO: port to dabbrevs
(defun disapproval () (interactive) (insert "ಠ_ಠ"))
(defun eyeroll () (interactive) (insert "◔_◔"))
(defun tables () (interactive) (insert "（╯°□°）╯︵ ┻━┻"))
(defun mu () (interactive) (insert "無"))

(when (and window-system (>= emacs-major-version 23))
  (let ((fontset (face-attribute 'default :fontset))
        (unifont "-gnu-unifont-medium-r-normal--13-120-75-75-c-0-iso10646-1"))
    (mapc
     (lambda (x)
       (set-fontset-font fontset (car x) (cdr x) nil))
     `(((#x02000 . #x026ff) . ,unifont )
       ((#x0210e . #x0210f) . "Unicode")
       ((#x02700 . #x028ff) . "Unicode")
       ((#x1f300 . #x1f6ff) . "Unicode"))) ))
