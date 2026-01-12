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
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  ;; Show fine differences for all displayed diff hunks
  ;; 显示所有差异块的细微差异
  (setq magit-diff-refine-hunk 'all))

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
