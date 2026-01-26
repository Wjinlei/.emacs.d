;;; init-base.el --- Core functionality configuration / 核心功能配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Base packages for better Emacs performance and behavior.
;; 用于改善 Emacs 性能或行为的包。
;;

;;; Code:

;; Disable startup screen / 禁用启动画面
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Force exit without save prompt / 强制退出不提示保存
;; Only use :w to save, all other exits are forced / 只用 :w 保存，其他退出都是强制的
(defun mint/force-save-buffers-kill-emacs ()
  "Exit Emacs without prompting to save. / 退出 Emacs 不提示保存。"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (set-buffer-modified-p nil)))
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'mint/force-save-buffers-kill-emacs)

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

;; Recent files / 最近文件
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300
        recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                          "\\.?ido\\.last$" "\\.revive$" "/G]ETTAGS$" "/.elfeed/"
                          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  ;; Silent save / 静默保存
  (setq recentf-auto-cleanup 'never))

(provide 'init-base)

;;; init-base.el ends here
