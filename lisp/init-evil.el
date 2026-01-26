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
  (define-key evil-normal-state-map (kbd "q") 'mint/force-kill-buffer)
  (define-key evil-visual-state-map (kbd "q") 'mint/force-kill-buffer)
  ;; Q force quit without save prompt / Q 强制退出不提示保存
  (defun mint/force-kill-buffer ()
    "Kill buffer without save prompt. / 强制关闭 buffer 不提示保存。"
    (interactive)
    (set-buffer-modified-p nil)
    (kill-buffer-and-window))
  (define-key evil-normal-state-map (kbd "Q") 'mint/force-kill-buffer)
  (define-key evil-visual-state-map (kbd "Q") 'mint/force-kill-buffer)
  ;; Quick file/buffer access / 快速文件/buffer访问
  (define-key evil-normal-state-map (kbd "ff") 'find-file)
  (define-key evil-normal-state-map (kbd "bb") 'consult-buffer)
  ;; Search keybindings / 搜索快捷键
  (define-key evil-normal-state-map (kbd "fs") 'consult-ripgrep)      ; Search files / 搜索项目下的文件
  (define-key evil-normal-state-map (kbd "fl") 'consult-line)         ; Search buffer (line) / 当前buffer搜索
  (define-key evil-normal-state-map (kbd "fg") 'consult-git-grep)     ; Search git files / 搜索git文件(更快) 
  (define-key evil-normal-state-map (kbd "fh") 'consult-recent-file) ; History files / 历史文件
  (define-key evil-normal-state-map (kbd "ft") 'mint-switch-theme)   ; Switch theme / 切换主题
  ;; C-j/C-k: move 5 lines or navigate corfu menu / C-j/C-k 移动5行或导航补全菜单
  (defun mint/corfu-visible-p ()
    "Check if corfu popup is visible."
    (and (boundp 'corfu--frame)
         corfu--frame
         (frame-live-p corfu--frame)
         (frame-visible-p corfu--frame)))
  (defun mint/smart-c-j ()
    "Move 5 lines down, or next item in corfu menu."
    (interactive)
    (if (mint/corfu-visible-p)
        (corfu-next)
      (evil-next-line 5)))
  (defun mint/smart-c-k ()
    "Move 5 lines up, or previous item in corfu menu."
    (interactive)
    (if (mint/corfu-visible-p)
        (corfu-previous)
      (evil-previous-line 5)))
  (define-key evil-normal-state-map (kbd "C-j") 'mint/smart-c-j)
  (define-key evil-normal-state-map (kbd "C-k") 'mint/smart-c-k)
  (define-key evil-visual-state-map (kbd "C-j") 'mint/smart-c-j)
  (define-key evil-visual-state-map (kbd "C-k") 'mint/smart-c-k)
  (define-key evil-insert-state-map (kbd "C-j") 'mint/smart-c-j)
  (define-key evil-insert-state-map (kbd "C-k") 'mint/smart-c-k)
  ;; Macro recording / 宏录制，按两下x开始录制，再按一次x停止录制，按大写X执行宏
  ;; x -> start/stop recording macro (like vim's q) / 开始/停止录制宏
  (define-key evil-normal-state-map (kbd "x") 'evil-record-macro)
  (define-key evil-normal-state-map (kbd "X") (kbd "@x")))

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
        evil-escape-delay 0.2
        evil-escape-excluded-major-modes '(magit-mode
                                           magit-status-mode
                                           magit-diff-mode
                                           magit-log-mode
                                           magit-revision-mode
                                           magit-process-mode))
  :config
  (evil-escape-mode 1))

(provide 'init-evil)

;;; init-evil.el ends here
