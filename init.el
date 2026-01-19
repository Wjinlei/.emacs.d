;;; init.el --- Emacs configuration entry point / 配置入口 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Mint Emacs - A minimal, coding-focused Emacs configuration.
;; Mint Emacs - 一个专注于编码的精简 Emacs 配置。
;;

;;; Code:

;; Version check / 版本检查
(when (version< emacs-version "28.1")
  (error "This requires Emacs 28.1 and above! / 需要 Emacs 28.1 及以上版本！"))

;; Speed up startup / 加速启动
(setq gc-cons-threshold most-positive-fixnum)

;; Add lisp directory to load-path / 添加 lisp 目录到加载路径
(defun mint--update-load-path (&rest _)
  "Update `load-path'. / 更新加载路径。"
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(advice-add #'package-initialize :after #'mint--update-load-path)
(mint--update-load-path)

;; Initialize package system / 初始化包管理系统
(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package / 初始化 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

;; Load custom variables first / 首先加载自定义变量
(require 'init-custom)

;; Load custom.el (user customizations)
;; 加载 custom.el（用户自定义配置）
;; If custom.el doesn't exist, copy from custom-example.el
;; 如果 custom.el 不存在，从 custom-example.el 复制
(let ((custom-example (expand-file-name "custom-example.el" user-emacs-directory)))
  (unless (file-exists-p custom-file)
    (when (file-exists-p custom-example)
      (copy-file custom-example custom-file)))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Load core functionality / 加载基础功能
(require 'init-base)

;; Load UI/Theme / 加载 UI/主题
(require 'init-ui)

;; Load Evil mode / 加载 Evil 模式
(require 'init-evil)

;; Load VCS (Git) / 加载版本控制 (Git)
(require 'init-vcs)

;; Load Dired / 加载文件管理
(require 'init-dired)

;; Restore GC threshold after startup / 启动完成后恢复 GC 阈值
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

;;; init.el ends here
