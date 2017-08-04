;;
;; brandon's dot emacs
;;

(setq user-full-name "Brandon van Beekum")

;; path
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; colors
(setq ns-use-srgb-colorspace nil)

;; modeline
(require 'spaceline-config)
(setq powerline-default-separator 'wave)
(setq display-time-day-and-date t)
(setq display-time-load-average-threshold 2.0)
(setq display-time-24hr-format t)
(display-time-mode 1)
(setq spaceline-buffer-encoding-abbrev-p nil)
(spaceline-emacs-theme)

;; (spaceline-spacemacs-theme)

;; themes
(setq custom-safe-themes t)
(tw)

;; bell
(setq visual-bell nil)
(setq ring-bell-function 'ignore)

;; global key bindings
;;

(fset 'copy-word
      [?\C-\M-  ?\M-w])

(fset 'kill-yank
      [?\C-k ?\C-y])

(define-key global-map (kbd "M-s-s")
  (lambda () (interactive) (insert "ß")))
(define-key global-map (kbd "M-s-u")
  (lambda () (interactive) (insert "ü")))
(define-key global-map (kbd "M-s-a")
  (lambda () (interactive) (insert "ä")))
(define-key global-map (kbd "M-s-o")
  (lambda () (interactive) (insert "ö")))
(define-key global-map (kbd "M-s-e")
  (lambda () (interactive) (insert "é")))

(global-set-key [mouse-3] 'other-window)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "s-[") 'previous-multiframe-window)
(define-key global-map (kbd "s-]") 'next-multiframe-window)
(define-key global-map (kbd "s-b") 'ido-switch-buffer)
(define-key global-map (kbd "s-i") 'copy-word)
(define-key global-map (kbd "s-m") 'ido-find-file)
(define-key global-map (kbd "s-'") 'magit-status)
(define-key global-map (kbd "s-;") 'find-file-in-project)
(define-key global-map (kbd "s-4") 'imenu)
(define-key global-map (kbd "s--") 'kill-yank)
(define-key global-map (kbd "s-d") 'kill-whole-line)
(define-key global-map [s-up] 'delete-other-windows)
(define-key global-map [s-down] 'delete-window)
(define-key global-map [f6] 'mark-sexp)
(define-key global-map (kbd "M-s-˚") 'highlight-symbol-at-point)
(define-key global-map (kbd "M-s-¬") 'unhighlight-regexp)

;; paredit bindings
;;

(define-key global-map (kbd "M-[") 'paredit-wrap-square)
(define-key global-map (kbd "M-{") 'paredit-wrap-curly)

;; global bindings for a few commonly opened files
;;

(global-set-key (kbd "M-s-ç")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-s-†")
                (lambda () (interactive) (find-file "~/lang/org/foo/todo.org")))
(global-set-key (kbd "M-s-µ")
                (lambda () (interactive) (find-file "~/lang/org/zalando/mask.org")))
(global-set-key (kbd "M-s-˙")
                (lambda () (interactive) (magit-status "~/zalando/shop")))

;; disable flyspell
;;

(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;; locks, backup and auto-save
;;

(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups" t)))

;; font and cosmetics
;;

(tool-bar-mode -1)
(set-default-font "Noto Mono 15")
;; (set-default-font "Monaco 15")
;; (set-default-font "Roboto Mono" 15)
;; (set-default-font "Roboto Mono Light" 15)
;; (set-default-font "Inconsolata 18")
;; (set-default-font "Inconsolata-g g")
;; (set-default-font "Source Code Pro 18")
;; (set-default-font "Source Code Pro Medium")
;; (set-default-font "Source Code Pro Light 16")

;; eww
;;

(defadvice shr-color-check (before unfuck compile activate)
  "Don't let stupid shr change background colors."
  (setq bg (face-background 'default)))

;; mode line tweaks
;;

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "CLJ")
(rename-modeline "cider-mode" cider-mode "C")

(setq cider-mode-line-show-connection nil)

;; buffer switching
;;

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key [s-backspace] 'switch-to-other-buffer)

;; buffer-move
;;

;; (require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; mark movement
;;

(require 'marker-visit)
(define-key global-map (kbd "s-5") 'marker-visit-prev)
(define-key global-map (kbd "s-6") 'marker-visit-next)

;; magit
;;

(setq magit-last-seen-setup-instructions "1.4.0")

;; ido
;;

(setq ido-use-filename-at-point nil)

;; c
;;

(setq-default c-basic-offset 4)

;; css
;;

(setq css-indent-offset 2)

;; html, jsp, etc
;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; org
;;

(setq org-agenda-files (list "~/lang/org/foo/todo.org"))
(setq org-startup-folded nil)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-src-fontify-natively t)
(setq org-footnote-auto-label nil)
(setq org-latex-default-figure-position "!htb")
(setq org-export-babel-evaluate nil)
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file t)))

(defun my-org-publish-buffer-and-open ()
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file))
  (let* ((proj (org-publish-get-project-from-filename buffer-file-name))
         (proj-plist (cdr proj))
         (rel (file-relative-name buffer-file-name
                                  (plist-get proj-plist :base-directory)))
         (dest (plist-get proj-plist :publishing-directory))
         (proj-img (replace-regexp-in-string "-org" "-img" (car proj))))
    (message "foo: %s -> %s" (car proj) proj-img)
    (org-publish-project proj-img)
    ;; (org-publish-attachment proj-plist buffer-file-name dest)
    (browse-url (concat "file://"
                        (file-name-as-directory (expand-file-name dest))
                        (file-name-sans-extension rel)
                        ".html"))))

;; (expand-file-name (file-name-nondirectory "test.org") "~/Dropbox/pub/org")

(defun my-org-publish-project ()
  (interactive)
  (save-excursion
    (org-babel-execute-src-block)
    (org-publish-current-project)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key [s-return] 'my-org-publish-project)
            (local-set-key (kbd "s-/") 'my-org-publish-buffer)
            (local-set-key (kbd "s-\\") 'my-org-publish-buffer-and-open)
            (local-set-key (kbd "M-s-π") 'org-publish-current-project)
            (local-set-key (kbd "M-s-¬") 'org-latex-export-to-pdf)
            (local-set-key (kbd "s-.") 'org-goto)))

(defun my-org-create (path)
  (list
   (list (concat path "-org")
         :publishing-function 'org-twbs-publish-to-html
         :with-sub-superscript nil
         :with-headline-numbers nil
         :html-head-extra "
<style>
  pre {
    color: #fff;
    background-color: #000;
    border-radius: 0;
  }
</style>"
         :base-directory (concat "~/lang/org/" path "/")
         :publishing-directory (concat "~/Dropbox/pub/" path))
   (list (concat path "-img")
         :base-directory (concat "~/lang/org/" path "/img")
         :base-extension "png"
         :recursive t
         :publishing-function 'org-publish-attachment
         :publishing-directory (concat "~/Dropbox/pub/" path "/img"))
   ))

;; (list path :components (list (concat path "-org") (concat path "-img")))

(setq my-org-dirs '("prog" "grow" "jobs" "zalando" "webtrekk"))

(setq org-publish-project-alist
      (mapcan #'my-org-create my-org-dirs))

;; comint
;;

(defun my-comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key comint-mode-map "\C-c\M-o" #'my-comint-clear-buffer)

;; flycheck
;;

(global-set-key (kbd "M-s-≥") 'next-error)
(global-set-key (kbd "M-s-≤") 'previous-error)

;; javascript
;;

(defun vv-run-node-file ()
  (interactive)
  (save-buffer)
  (async-shell-command (concat "node " (buffer-file-name))))

(add-hook 'js2-mode-hook
          (lambda ()
            (flycheck-mode 1)
            (local-set-key (kbd "s-/") 'js-send-last-sexp)
            (local-set-key (kbd "s-.") 'js-send-current-line)
            (local-set-key (kbd "s-\\") 'js-send-buffer)
            (local-set-key "\C-cl" 'js-load-file-and-go)
            (local-set-key [s-return] 'vv-run-node-file)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-global-externs
              '("module" "require" "setTimeout" "clearTimeout"
                "setInterval" "clearInterval" "location" "console" "JSON" "goog"))

(custom-set-variables '(coffee-tab-width 2))
(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq js2-indent-switch-body t)

;; let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
;; (setq js2-language-version 170)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq flycheck-checkers '(javascript-eslint))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  )

;; standard-ml
;;

(setq sml-program-name "/usr/local/smlnj/bin/sml")
(add-hook 'sml-mode-hook
          (lambda ()
            (local-set-key [f5] 'sml-prog-proc-switch-to)
            (local-set-key (kbd "s-\\") 'sml-prog-proc-send-buffer)))

;; elisp
;;

(defun debug-mode ()
  "Turn on various Emacs debugging features"
  (interactive)
  (setf debug-on-error t message-log-max 10000))

;; cannot get to work without making help buffer visible
;; http://blog.jenkster.com/2013/12/popup-help-in-emacs-lisp.html
(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun describe-thing ()
  (interactive)
  (let* ((thing (symbol-at-point)))
    (switch-to-buffer "*Help*")
    (help-xref-interned thing)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'eval-buffer)
            (local-set-key (kbd "s-.") 'eval-defun)
            (local-set-key (kbd "s-/") 'eval-last-sexp)
            (local-set-key (kbd "s-=") 'describe-thing)))

;; graphviz
;;

(fset 'gv
      [?\C-c ?c return ?\C-c ?p])

(fset 'gg
      [?\C-c ?c return ?\M-: ?( ?s ?l ?e ?e ?p ?- ?f ?o ?r ? ?1 ?) return ?\C-c ?p])

(add-hook 'graphviz-dot-mode-hook
          (lambda ()
            (local-set-key [f5] 'gg)))

;; mozart/oz
;;

(add-hook 'oz-mode-hook
          (lambda ()
            (local-set-key [f5] 'oz-feed-buffer)))

;; haskell
;;

(defun my-hs-run-main ()
  "Switch to intero repl Haskell buffer and run main"
  (interactive)
  (intero-repl-load)
  (insert "someFunc")
  (comint-send-input)
  (intero-repl-switch-back))

(defun my-hs-load ()
  (interactive)
  (intero-repl-load)
  (intero-repl-switch-back))

;; (with-current-buffer "*haskell*"
;;     (insert "main")
;;     (comint-send-input))

(use-package intero
  :ensure t
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  :bind (:map intero-mode-map
              ("<f5>" . intero-repl)
              ("s-=" . intero-type-at)
              ("s-\\" . my-hs-load)
              ([s-return] . my-hs-run-main)))

;; (custom-set-variables '(haskell-tags-on-save t))

;; (eval-after-load "hhhaskell-mode"
;;   '(custom-set-variables
;;     '(haskell-mode-hook
;;       '(turn-on-haskell-doc-mode
;;         ghc-init
;;         hindent-mode
;;         ;; structured-haskell-mode
;;         (lambda ()
;;           (local-set-key [f5] 'inferior-haskell-load-file)
;;           (local-set-key [M-f6] 'inferior-haskell-type)
;;           (local-set-key (kbd "s-=") 'inferior-haskell-info)
;;           (local-set-key [s-return] 'my-hs-run-main)
;;           (local-set-key (kbd "s-s") 'ghc-save-buffer))))))

;; (defun my-hs-run-main ()
;;   "Switch to inferior Haskell buffer and run main"
;;   (interactive)
;;   (with-current-buffer "*haskell*"
;;     (insert "main")
;;     (comint-send-input)))

;; python
;;

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'python-shell-send-buffer)))

;; scheme
;;

(require 'xscheme)
(setq scheme-program-name "/usr/local/bin/mit-scheme")
(add-hook 'scheme-mode-hook
          (lambda ()
            (local-set-key [f5] 'run-scheme)
            (local-set-key [M-f6] 'reset-scheme)
            (local-set-key (kbd "s-.") 'xscheme-send-definition)
            (local-set-key (kbd "s-/") 'xscheme-send-previous-expression)
            (local-set-key (kbd "s-\\") 'xscheme-send-buffer)))

;; markdown
;;

(defun markdown-buffer-to-dropbox ()
  (interactive)
  (save-buffer)
  (let ((f buffer-file-name))
    (when (and (> (string-width f) 0)
               (string= "md" (file-name-extension f)))
      (let ((cmd (concat "~/bin/domark"
                         " " buffer-file-name " "
                         "~/Dropbox/md/"
                         (file-name-nondirectory f))))
        (message (shell-command-to-string cmd))))))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "s-\\") 'markdown-buffer-to-dropbox)))

;; html
;;

(defun html-hook ()
  (local-set-key "\C-m" (lambda () (interactive) (insert "\n"))))

(setq sgml-mode-hook 'html-hook)

;; clojure
;;

;; lisp-body-indent
(setq clojure-indent-style :always-align) ; oberyn
;; (setq clojure-indent-style :align-arguments) ; clj-commoner
;; (setq clojure-indent-style :always-indent) ; baelish

(setq cider-save-file-on-load t)

;; (require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; (defun run-tests-for-file ()
;;   (clojure-jump-between-tests-and-code)
;;   (clojure-test-run-tests))

;; buffer stuff
;; (message "msg: %s" (clojure-project-dir (cider-current-dir)))

(defun my-find-nrepl ()
  (interactive)
  (let ((messages-buffer-name (buffer-name (cider-current-messages-buffer))))
    (replace-regexp-in-string "messages" "server" messages-buffer-name)))

(defun my-switch-to-nrepl ()
  (interactive)
  (display-buffer (my-find-nrepl)))

(defun my-switch-to-repl ()
  (interactive)
  (display-buffer (cider-current-repl-buffer)))

(defun my-clj-logv ()
  (interactive)
  (with-current-buffer (my-find-nrepl)
    (beginning-of-buffer)
    (kill-line)
    (delete-char 1)
    (end-of-buffer)
    (logview-mode)
    (text-scale-decrease 2)
    (read-only-mode -1)))

;; buffer stuff end
;;

(defun my-clojure-debug ()
  (interactive)
  (cider-debug-defun-at-point))

(add-hook 'clojure-mode-hook
          (lambda ()
            (hs-minor-mode)
            (local-set-key (kbd "s-0") 'hs-hide-all)
            (local-set-key (kbd "s-9") 'hs-show-all)
            (local-set-key (kbd "s-8") 'hs-hide-block)
            (local-set-key (kbd "s-7") 'hs-show-block)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "M-s-©") 'my-clojure-debug)
            (local-set-key (kbd "M-s-i") 'cider-inspect-last-sexp)
            (local-set-key (kbd "s-=") 'ac-cider-popup-doc)
            (local-set-key (kbd "s-.") 'cider-eval-defun-at-point)
            (local-set-key (kbd "s-/") 'cider-eval-last-sexp)
            (local-set-key (kbd "M-s-÷") 'cider-pprint-eval-last-sexp)
            (local-set-key [f5] 'my-switch-to-repl)
            (local-set-key [M-f5] 'my-switch-to-nrepl)
            (local-set-key (kbd "M-s-<f6>") 'my-clj-logv)
            (local-set-key [M-f6] 'cider-restart)
            (local-set-key [M-S-return] 'cider-eval-buffer)
            (local-set-key (kbd "M-s-∆") 'cider-test-run-ns-tests)
            ;; (local-set-key (kbd "M-s-l") 'run-tests-for-file)
            ;; (local-set-key (kbd "M-s-k") 'clojure-jump-between-tests-and-code)
            (local-set-key [s-return] 'cider-repl-set-ns)
            (local-set-key (kbd "s-\\") 'cider-load-file)))

(add-hook 'cider-mode-hook 'eldoc-mode)
(setq cider-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(setq cider-repl-display-help-banner nil)

;; (view-buffer-other-window "*nrepl-server oberyn*")
;; (switch-to-buffer-other-window "*nrepl-server oberyn*")
;; (display-buffer-in-major-side-window)
;; (display-buffer "*nrepl-server oberyn*")

;; racket

(add-hook 'racket-mode-hook
          (lambda ()
            (local-set-key [f5] 'racket-run-and-switch-to-repl)
            (local-set-key (kbd "s-\\") 'racket-run)
            (local-set-key (kbd "s-/")  'racket-send-last-sexp)
            (local-set-key (kbd "s-.")  'racket-send-definition)
            (local-set-key (kbd "s-=")  'racket-describe)
            (local-set-key (kbd "s-9")  'racket-expand-last-sexp)
            (local-set-key (kbd "s-0")  'racket-expand-again)))

;; auto-complete
;;

(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)
     (add-to-list 'ac-modes 'sml-mode)
     (add-to-list 'ac-modes 'js2-mode)
     (add-to-list 'ac-modes 'emacs-lisp-mode)
     (add-to-list 'ac-modes 'racket-mode)))

(setq ac-auto-start nil)

(require 'auto-complete-config)
(ac-config-default)

;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))

;; (require 'ac-cider)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook
          'set-auto-complete-as-completion-at-point-function)

;; latest
;;

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
  :interpreter ("scala" . scala-mode)
  :ensure t
  :defer t
  :init
  (setq scala-indent:align-parameters t)
  :config
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (prettify-symbols-mode))

;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el

;; ("C-c m E" . ensime-reload)
;; ;; Free M-n and M-p again
;; ("M-n" . nil)
;; ("M-p" . nil)
;; ("<f5>" . ensime-sbt-do-compile)
;; :map scala-mode-map ("C-c m e" . ensime)))

(use-package ensime
  :ensure t
  :after scala-mode
  :bind (:map ensime-mode-map
              ("s-/" . ensime-inf-switch)
              ("<f5>" . ensime-type-at-point)
              ("s-=" . ensime-print-errors-at-point)
              ("M-s-¬" . ensime-inspect-type-at-point)
              ("C-<f5>" . ensime-show-all-errors-and-warnings)
              :map scala-mode-map
              ("C-c m e" . ensime)))

(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)

(add-hook 'find-file-hook
          (lambda ()
            (setq ffip-prune-patterns (append ffip-prune-patterns
                                              '("*/target/*"
                                                "*/.ensime_cache/*"
                                                "*/.stack-work/*")))))
