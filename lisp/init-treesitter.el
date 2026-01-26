;;; init-treesitter.el --- Tree-sitter configuration / Tree-sitter配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tree-sitter support for better syntax highlighting and code analysis.
;; Tree-sitter 支持，提供更好的语法高亮和代码分析。
;;

;;; Code:

;; Check if tree-sitter is available (Emacs 29+)
(defun mint-treesitter-available-p ()
  "Check if tree-sitter is available. / 检查 tree-sitter 是否可用。"
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

;; Tree-sitter configuration
(when (mint-treesitter-available-p)
  ;; Automatic Tree-sitter grammar management / 自动管理 Tree-sitter 语法
  (use-package treesit-auto
    :hook (after-init . global-treesit-auto-mode)
    :init
    (setq treesit-auto-install 'prompt)
    :config
    ;; Remap major modes to tree-sitter modes / 将主模式映射到 tree-sitter 模式
    (treesit-auto-add-to-auto-mode-alist 'all))

  ;; Code folding with tree-sitter / 使用 tree-sitter 进行代码折叠
  (use-package treesit-fold
    :hook (after-init . global-treesit-fold-mode)
    :bind (:map prog-mode-map
           ("C-c f t" . treesit-fold-toggle)
           ("C-c f c" . treesit-fold-close)
           ("C-c f o" . treesit-fold-open)
           ("C-c f a" . treesit-fold-open-all)
           ("C-c f r" . treesit-fold-close-all))))

;; Built-in treesit configuration / 内置 treesit 配置
(use-package treesit
  :ensure nil
  :when (mint-treesitter-available-p)
  :init
  ;; Font-lock level: 1-4, higher = more colorful / 字体锁定级别，越高颜色越丰富
  (setq treesit-font-lock-level 4)
  ;; Extra source for grammars / 语法源
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (vue        . ("https://github.com/ikatyang/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml")))))

(defun mint-treesitter-install-all-grammars ()
  "Install all tree-sitter grammars. / 安装所有 tree-sitter 语法。"
  (interactive)
  (if (mint-treesitter-available-p)
      (progn
        (message "Installing tree-sitter grammars... / 正在安装 tree-sitter 语法...")
        (mapc #'treesit-install-language-grammar
              (mapcar #'car treesit-language-source-alist))
        (message "Tree-sitter grammars installed! / Tree-sitter 语法安装完成！"))
    (user-error "Tree-sitter is not available in this Emacs build. / 此 Emacs 版本不支持 tree-sitter。")))

(provide 'init-treesitter)

;;; init-treesitter.el ends here
