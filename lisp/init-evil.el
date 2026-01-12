;;; init-evil.el --- Evil mode configuration / Evil 模式配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Evil mode - Vim emulation for Emacs.
;; Evil 模式 - 在 Emacs 中模拟 Vim 操作。
;;

;;; Code:

;; Evil mode / Vim 模拟模式
(use-package evil
  :ensure t
  :demand t  ; Force immediate loading / 强制立即加载
  :init
  ;; Required for evil-collection / evil-collection 需要这些设置
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)
  :config
  ;; Enable evil mode globally / 全局启用 evil 模式
  (evil-mode 1)
  ;; Use Emacs state in these modes / 在这些模式下使用 Emacs 状态
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode
                  calculator-mode
                  dired-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Evil collection - additional keybindings / 额外的按键绑定
(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :config
  (evil-collection-init))

;; Evil escape - use jj to escape / 使用 jj 退出插入模式
(use-package evil-escape
  :ensure t
  :after evil
  :demand t
  :init
  (setq evil-escape-key-sequence "jj"
        evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

(provide 'init-evil)

;;; init-evil.el ends here
