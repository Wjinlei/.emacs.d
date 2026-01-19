;;; init-vcs.el --- Version control configuration / 版本控制配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Git integration with Magit.
;; 使用 Magit 集成 Git。
;;

;;; Code:

;; Declare magit variables to avoid byte-compile warnings
;; 声明 magit 变量以避免字节编译警告
(defvar magit-auto-revert-mode nil)

;; Magit - Git interface / Git 界面
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-file-dispatch
             magit-clone magit-init magit-blame
             magit-log-buffer-file magit-diff
             magit-stage-file magit-unstage-file)
  :bind (;; Basic bindings / 基础绑定
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g c" . magit-clone)
         ("C-c g i" . magit-init)
         ("C-c g m" . magit-dispatch)
         ("C-c g d" . magit-diff)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g S" . magit-stage-file)
         ("C-c g U" . magit-unstage-file)
         ("C-c g f" . magit-file-dispatch))
  :config
  ;; Show fine differences for all displayed diff hunks
  ;; 显示所有差异块的细微差异
  (setq magit-diff-refine-hunk 'all)
  ;; Restore window configuration when quitting magit (spacemacs style)
  ;; 退出magit时恢复窗口配置（spacemacs风格）
  (setq magit-bury-buffer-function #'magit-restore-window-configuration))

;; Show git changes in the gutter / 在边栏显示 git 更改
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Use margin instead of fringe in terminal / 终端下使用边距而非边缘
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(provide 'init-vcs)

;;; init-vcs.el ends here
