;;
;; init.el - configure emacs
;;

(setq debug-on-error t)
(setq exec-path-from-shell-check-startup-files nil)
(setq ensime-startup-notification nil)

;; setup package manager
;;

(require 'package)

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(message "pre..")
(package-initialize)
(message "post..")

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(smex exec-path-from-shell use-package
         magit paredit auto-complete graphviz-dot-mode
         rainbow-delimiters buffer-move xscheme
         sml-mode shm hindent
         monokai-theme zenburn-theme twilight-anti-bright-theme
         diminish flycheck js2-mode rainbow-mode
         web-mode less-css-mode markdown-mode
         clojure-mode clojure-mode-extra-font-locking
         cider ac-cider spaceline popup-imenu
         intero logview yaml-mode ox-twbs)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (set-frame-size nil 240 80)
;; (set-frame-size nil 282 80) ;; imac - full-screen
(set-frame-size nil 215 80) ;; imac - 3/4
;; (set-frame-size nil 170 60) ;; middle
;; (set-frame-size nil 245 80) ;; thunder - 4/5
(setq ns-auto-hide-menu-bar t)
;; (set-frame-position nil 0 0)
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(setq datetime-timezone "GMT+1")
;; (current-time-zone)
(save-place-mode 1)
(setq default-tab-width 4)

(add-hook 'window-setup-hook
          (lambda() (set-frame-position nil 0 -2)))

