;;; init-funcs.el --- Utility functions / 工具函数 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Utility functions and install commands.
;; 工具函数和安装命令。
;;

;;; Code:

(require 'cl-lib)

;;; Install Commands / 安装命令

(defvar mint-required-packages
  '(;; Completion / 补全
    vertico
    orderless
    marginalia
    consult
    consult-dir
    corfu
    cape
    nerd-icons-corfu
    nerd-icons-completion
    ;; LSP / 语言服务
    consult-eglot
    ;; Go
    go-mode
    gotest
    go-tag
    ;; Web
    web-mode
    typescript-mode
    json-mode
    scss-mode
    emmet-mode
    add-node-modules-path
    ;; Evil
    evil
    evil-collection
    evil-surround
    evil-nerd-commenter
    ;; Git
    magit
    diff-hl
    ;; UI
    doom-themes
    doom-modeline
    nerd-icons
    ;; Highlight / 高亮
    rainbow-delimiters
    hl-todo
    symbol-overlay
    colorful-mode
    indent-bars
    goggles
    ;; Tree-sitter
    treesit-auto
    treesit-fold
    ;; Edit / 编辑
    vundo
    goto-chg
    iedit
    multiple-cursors
    expand-region
    mwim
    hungry-delete
    editorconfig
    ;; Snippet / 代码片段
    yasnippet
    yasnippet-snippets
    ;; Misc
    which-key
    helpful
    restart-emacs)
  "List of required packages. / 必需的包列表。")

(defun mint-install-packages ()
  "Install all required packages. / 安装所有必需的包。"
  (interactive)
  (message "Refreshing package contents... / 正在刷新包列表...")
  (package-refresh-contents)
  (message "Installing packages... / 正在安装包...")
  (dolist (pkg mint-required-packages)
    (unless (package-installed-p pkg)
      (condition-case err
          (progn
            (package-install pkg)
            (message "Installed: %s" pkg))
        (error (message "Failed to install %s: %s" pkg (error-message-string err))))))
  (message "Package installation complete! / 包安装完成！"))

(defun mint-install-go-tools ()
  "Install Go development tools. / 安装 Go 开发工具。"
  (interactive)
  (if (fboundp 'mint-go-install-tools)
      (mint-go-install-tools)
    (message "Go module not loaded. Please load init-go first. / Go 模块未加载。")))

(defun mint-install-vue-tools ()
  "Install Vue/Volar language server. / 安装 Vue/Volar 语言服务器。"
  (interactive)
  (if (executable-find "npm")
      (progn
        (message "Installing Vue language server... / 正在安装 Vue 语言服务器...")
        (let ((buf (get-buffer-create "*Vue Tools Install*")))
          (display-buffer buf)
          (set-process-sentinel
           (start-process "vue-tools" buf "npm" "install" "-g" "@vue/language-server")
           (lambda (proc _)
             (if (= 0 (process-exit-status proc))
                 (message "Vue language server installed! / Vue 语言服务器安装完成！")
               (message "Failed to install Vue language server. / Vue 语言服务器安装失败。"))))))
    (user-error "npm not found! Please install Node.js first. / 未找到 npm，请先安装 Node.js。")))

(defun mint-install-nerd-icons ()
  "Install Nerd Icons fonts. / 安装 Nerd Icons 字体。"
  (interactive)
  (if (fboundp 'nerd-icons-install-fonts)
      (nerd-icons-install-fonts t)
    (message "nerd-icons not loaded. / nerd-icons 未加载。")))

(defun mint-install-all ()
  "Install all packages and tools. / 安装所有包和工具。"
  (interactive)
  (mint-install-packages)
  (when (yes-or-no-p "Install Go tools? / 是否安装 Go 工具？")
    (mint-install-go-tools))
  (when (yes-or-no-p "Install Vue language server? / 是否安装 Vue 语言服务器？")
    (mint-install-vue-tools))
  (when (yes-or-no-p "Install Nerd Icons fonts? / 是否安装 Nerd Icons 字体？")
    (mint-install-nerd-icons))
  (message "All installations complete! / 所有安装完成！"))

;;; Utility Functions / 工具函数

(defun mint-reload-init ()
  "Reload Emacs configuration. / 重新加载 Emacs 配置。"
  (interactive)
  (load user-init-file))

(defun mint-open-init-file ()
  "Open init.el file. / 打开 init.el 文件。"
  (interactive)
  (find-file user-init-file))

(defun mint-open-custom-file ()
  "Open custom.el file. / 打开 custom.el 文件。"
  (interactive)
  (find-file custom-file))

(provide 'init-funcs)

;;; init-funcs.el ends here
