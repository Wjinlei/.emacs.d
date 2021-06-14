;; init-yasnippet.el

;; This is CentaurEmacs config with my private modificatons
;; https://github.com/seagle0128/.emacs.d/
;; It just adds Evil (vim) and some minor modifications to suit my needs 

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
