;;; init-go.el --- Go language configuration / Go语言配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Go language support with automatic tool installation.
;; Go 语言支持，包含自动工具安装。
;;

;;; Code:

;; Go tools list / Go 工具列表
(defvar mint-go-tools
  '("golang.org/x/tools/gopls"
    "golang.org/x/tools/cmd/goimports"
    "honnef.co/go/tools/cmd/staticcheck"
    "github.com/go-delve/delve/cmd/dlv"
    "github.com/fatih/gomodifytags"
    "github.com/cweill/gotests/..."
    "github.com/josharian/impl")
  "Essential Go tools to install. / 需要安装的 Go 工具列表。")

(defun mint-go-install-tools ()
  "Install or update Go tools. / 安装或更新 Go 工具。"
  (interactive)
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'! / 在 exec-path 中找不到 go！"))
  (message "Installing Go tools... / 正在安装 Go 工具...")
  (dolist (pkg mint-go-tools)
    (let ((buf (get-buffer-create "*Go Tools Install*")))
      (display-buffer buf)
      (set-process-sentinel
       (start-process "go-tools" buf "go" "install" "-v" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed: %s" pkg)
             (message "Failed to install: %s (exit code: %d)" pkg status))))))))

(defun mint-go-auto-install-tools ()
  "Automatically install Go tools if gopls is not found.
/ 如果找不到 gopls，自动安装 Go 工具。"
  (when (and (executable-find "go")
             (not (executable-find "gopls")))
    (when (yes-or-no-p "gopls not found. Install Go tools? / 未找到 gopls，是否安装 Go 工具？")
      (mint-go-install-tools))))

;; Go mode configuration / Go 模式配置
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . mint-go-auto-install-tools)
  :bind (:map go-mode-map
         ("C-c C-f" . gofmt))
  :config
  (setq gofmt-command "goimports")
  ;; Format on save / 保存时格式化
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                (gofmt-before-save)))))

;; Go tree-sitter mode (Emacs 29+) / Go tree-sitter 模式
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (use-package go-ts-mode
    :ensure nil
    :mode "\\.go\\'"
    :hook (go-ts-mode . mint-go-auto-install-tools)
    :init
    (setq go-ts-mode-indent-offset 4)))

;; Go test / Go 测试
(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
         ("C-c t f" . go-test-current-file)
         ("C-c t t" . go-test-current-test)
         ("C-c t p" . go-test-current-project)
         ("C-c t b" . go-test-current-benchmark)
         ("C-c t x" . go-run)))

;; Go tag management / Go 标签管理
(use-package go-tag
  :after go-mode
  :bind (:map go-mode-map
         ("C-c t a" . go-tag-add)
         ("C-c t r" . go-tag-remove))
  :init
  (setq go-tag-args (list "-transform" "camelcase")))

(provide 'init-go)

;;; init-go.el ends here
