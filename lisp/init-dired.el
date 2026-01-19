;;; init-dired.el --- Dired file manager configuration / Dired 文件管理配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dired and related packages for file management.
;; Dired 及相关文件管理包。
;;

;;; Code:

;; Global keybinding to open dired / 全局快捷键打开 dired
(global-set-key (kbd "C-c d") 'dired-jump)

;; Directory operations / 目录操作
(use-package dired
  :ensure nil
  :config
  ;; Guess a default target directory / 猜测默认目标目录
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively / 始终递归删除和复制
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first / 目录优先显示
  (setq dired-listing-switches "-alh --group-directories-first"))

;; Quick sort dired buffers / 快速排序 dired 缓冲区
(use-package dired-quick-sort
  :ensure t
  :after dired)

;; Show git info in dired / 在 dired 中显示 git 信息
(use-package dired-git-info
  :ensure t
  :after dired)

;; Allow rsync from dired buffers / 允许从 dired 缓冲区使用 rsync
(use-package dired-rsync
  :ensure t
  :after dired)

;; Colorful dired / 彩色 dired
(use-package diredfl
  :ensure t
  :diminish
  :hook (dired-mode . diredfl-mode))

;; Shows icons in dired / 在 dired 中显示图标
(use-package nerd-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))

;; Extra Dired functionality / 额外的 Dired 功能
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store$\\|^\\.git$\\|^\\.vscode$\\|^\\.elc$")))

(provide 'init-dired)

;;; init-dired.el ends here
