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

;; Custom keybindings / 自定义快捷键
(with-eval-after-load 'evil
  ;; J/K -> half page scroll / 半页滚动
  (define-key evil-normal-state-map (kbd "J") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "K") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "J") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "K") 'evil-scroll-up)
  ;; H/L -> word movement / 单词移动
  (define-key evil-normal-state-map (kbd "H") 'evil-backward-word-begin)
  (define-key evil-normal-state-map (kbd "L") 'evil-forward-word-end)
  ;; C-l -> end of line / 行尾
  (define-key evil-normal-state-map (kbd "C-l") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-l") 'evil-end-of-line)
  ;; C-h -> first non-blank / 行首（第一个非空白字符）
  (define-key evil-normal-state-map (kbd "C-h") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "C-h") 'evil-first-non-blank)
  ;; w -> save / 保存
  (define-key evil-normal-state-map (kbd "w") 'save-buffer)
  (define-key evil-visual-state-map (kbd "w") 'save-buffer)
  ;; W -> save and quit / 保存并退出
  (define-key evil-normal-state-map (kbd "W") 'save-buffers-kill-emacs)
  (define-key evil-visual-state-map (kbd "W") 'save-buffers-kill-emacs)
  ;; Macro recording / 宏录制
  ;; x -> start/stop recording macro (like vim's q) / 开始/停止录制宏
  (define-key evil-normal-state-map (kbd "x") 'evil-record-macro)
  ;; X -> execute macro in register x / 执行寄存器x中的宏
  (define-key evil-normal-state-map (kbd "X") (kbd "@x"))
  ;; Buffer/Emacs exit / 退出
  ;; q -> kill current buffer / 关闭当前buffer
  (define-key evil-normal-state-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
  (define-key evil-visual-state-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
  ;; Q -> quit emacs without saving / 强制退出emacs
  (define-key evil-normal-state-map (kbd "Q") 'kill-emacs)
  (define-key evil-visual-state-map (kbd "Q") 'kill-emacs))

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
