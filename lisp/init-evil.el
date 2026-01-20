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
                  calculator-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Custom keybindings / 自定义快捷键
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-.") 'consult-imenu)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-l") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "C-h") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "J") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "K") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "J") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "K") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "H") 'evil-backward-word-begin)
  (define-key evil-normal-state-map (kbd "L") 'evil-forward-word-end)
  (define-key evil-normal-state-map (kbd "w") 'save-buffer)
  (define-key evil-visual-state-map (kbd "w") 'save-buffer)
  (define-key evil-normal-state-map (kbd "W") 'save-buffer)
  (define-key evil-visual-state-map (kbd "W") 'save-buffer)
  (define-key evil-normal-state-map (kbd "q") 'kill-buffer-and-window)
  (define-key evil-visual-state-map (kbd "q") 'kill-buffer-and-window)
  (define-key evil-normal-state-map (kbd "Q") 'kill-buffer-and-window)
  (define-key evil-visual-state-map (kbd "Q") 'kill-buffer-and-window)
  ;; Quick file/buffer access / 快速文件/buffer访问
  (define-key evil-normal-state-map (kbd "ff") 'find-file)
  (define-key evil-normal-state-map (kbd "bb") 'consult-buffer))
  ;; Macro recording / 宏录制，按两下x开始录制，再按一次x停止录制，按大写X执行宏
  ;; x -> start/stop recording macro (like vim's q) / 开始/停止录制宏
  (define-key evil-normal-state-map (kbd "x") 'evil-record-macro)
  (define-key evil-normal-state-map (kbd "X") (kbd "@x"))

;; Evil collection - additional keybindings / 额外的按键绑定
(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :config
  ;; Only enable for specific modes to avoid conflicts
  ;; 仅为特定模式启用以避免冲突
  (setq evil-collection-mode-list
        '(dired
          magit
          ibuffer
          info
          help
          custom
          ediff
          compile
          comint))
  (evil-collection-init)

  ;; Dired keybindings using evil-define-key (spacemacs style)
  ;; 使用 evil-define-key 定义 Dired 快捷键（spacemacs 风格）
  (with-eval-after-load 'dired
    ;; Define helper functions (from spacemacs vinegar layer)
    ;; 定义辅助函数（来自 spacemacs vinegar 层）
    (defun mint/dired-back-to-top ()
      "Move to first file in dired. / 移动到第一个文件"
      (interactive)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (dired-move-to-filename)))
        (forward-line 1)))

    (defun mint/dired-jump-to-bottom ()
      "Move to last file in dired. / 移动到最后一个文件"
      (interactive)
      (goto-char (point-max))
      (while (and (not (bobp))
                  (not (dired-move-to-filename)))
        (forward-line -1)))

    (defun mint/dired-move-up ()
      "Move to previous file, stay on first if at top. / 移动到上一个文件"
      (interactive)
      (dired-previous-line 1)
      (when (bobp) (dired-next-line 1)))

    (defun mint/dired-move-down ()
      "Move to next file, stay on last if at bottom. / 移动到下一个文件"
      (interactive)
      (dired-next-line 1)
      (when (eobp) (dired-next-line -1)))

    ;; Navigation / 导航
    (evil-define-key 'normal dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file
      "j" 'mint/dired-move-down
      "k" 'mint/dired-move-up
      "o" 'dired-find-file-other-window
      "r" 'revert-buffer
      "gg" 'mint/dired-back-to-top
      "G" 'mint/dired-jump-to-bottom
      ;; Editing / 编辑
      "i" 'wdired-change-to-wdired-mode
      "a" 'wdired-change-to-wdired-mode
      ;; Exit / 退出
      "q" 'quit-window
      "Q" 'quit-window
      ;; Search / 搜索
      "n" 'evil-search-next
      "N" 'evil-search-previous
      ;; Plugin keybindings / 插件快捷键
      "S" 'dired-quick-sort
      "." 'dired-git-info-mode
      (kbd "C-c C-r") 'dired-rsync)))

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
