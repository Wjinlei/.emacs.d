;;; init-base.el --- Core functionality configuration / 核心功能配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Base packages for better Emacs performance and behavior.
;; 用于改善 Emacs 性能或行为的包。
;;

;;; Code:

;; Garbage Collector Magic Hack / 垃圾回收优化
(use-package gcmh
  :ensure t
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init (setq gcmh-idle-delay 'auto
              gcmh-auto-idle-delay-factor 10
              gcmh-high-cons-threshold #x4000000)) ; 64MB

;; Environment / 环境变量
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Save place / 保存光标位置
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(provide 'init-base)

;;; init-base.el ends here
