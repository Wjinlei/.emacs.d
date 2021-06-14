;; init.el

;; This is CentaurEmacs config with my private modificatons
;; https://github.com/seagle0128/.emacs.d/
;; It just adds Evil (vim) and some minor modifications to suit my needs 

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold centaur-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold centaur-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold centaur-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-evil)
(require 'init-basic)
(require 'init-hydra)
;
(require 'init-ui)
(require 'init-edit)
(require 'init-ivy)
;(require 'init-company)
(require 'init-yasnippet)
;
;(require 'init-bookmark)
(require 'init-calendar)
;(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
;(require 'init-persp)
(require 'init-window)
;(require 'init-treemacs)
;
(require 'init-eshell)
(require 'init-shell)
;
(require 'init-markdown)
(require 'init-org)
;(require 'init-reader)
;
;(require 'init-docker)
(require 'init-utils)

;; Programming
(require 'init-vcs)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)
;
;(require 'init-prog)
;(require 'init-elisp)
(require 'init-c)
(require 'init-go)
;(require 'init-rust)
(require 'init-python)
;(require 'init-ruby)
;(require 'init-dart)
;(require 'init-elixir)
(require 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
