;;; init-edit.el --- Editing configuration / 编辑配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Editing enhancements including auto-indent, pair matching, etc.
;; 编辑增强，包括自动缩进、括号匹配等。
;;

;;; Code:

;; Delete selection when typing / 输入时删除选中内容
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatic parenthesis pairing / 自动括号配对
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Electric indent - smart context-aware indentation / 智能上下文缩进
(use-package electric
  :ensure nil
  :hook (after-init . electric-indent-mode)
  :init
  ;; Make RET also indent / 回车时也缩进
  (setq electric-indent-chars '(?\n ?\{ ?\})))

;; Indent settings for programming / 编程缩进设置
(setq-default indent-tabs-mode nil)  ; Use spaces, not tabs / 使用空格而非Tab
(setq-default tab-width 4)           ; Tab width / Tab宽度

;; Smart newline and indent / 智能换行缩进
(defun mint-newline-and-indent ()
  "Insert newline and indent according to major mode."
  (interactive)
  (newline)
  (indent-according-to-mode))

;; Bind RET to smart indent in prog-mode / 在编程模式中绑定回车到智能缩进
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") #'newline-and-indent)))

;; Automatically reload files modified externally / 自动重载外部修改的文件
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; Treat undo history as a tree / 撤销历史树
(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;; Goto last change / 跳转到上次修改
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Edit multiple regions simultaneously / 同时编辑多个区域
(use-package iedit
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)))

;; Multiple cursors / 多光标
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Expand region by semantic units / 按语义单元扩展选区
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Move to beginning/end of line or code / 移动到行首/行尾或代码
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; Hungry deletion / 饥饿删除
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Code folding / 代码折叠
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
         ("C-c h t" . hs-toggle-hiding)
         ("C-c h a" . hs-show-all)
         ("C-c h h" . hs-hide-all))
  :config
  ;; Display line counts when folded / 折叠时显示行数
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

;; Subword mode for CamelCase / 驼峰命名支持
(use-package subword
  :ensure nil
  :diminish
  :hook (prog-mode . subword-mode))

;; EditorConfig support / EditorConfig 支持
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Yasnippet - snippet expansion / 代码片段展开
;; Required for LSP function completion with parentheses / LSP 函数补全自动添加括号需要
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  ;; Disable default TAB binding to avoid conflict with corfu / 禁用默认 TAB 绑定避免与 corfu 冲突
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Use C-c y to expand snippets manually / 使用 C-c y 手动展开代码片段
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;; Yasnippet snippets collection / 代码片段集合
(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-edit)

;;; init-edit.el ends here
