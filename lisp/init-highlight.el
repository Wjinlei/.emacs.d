;;; init-highlight.el --- Code highlighting configuration / 代码高亮配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Syntax highlighting and visual enhancements.
;; 语法高亮和视觉增强。
;;

;;; Code:

;; Highlight the current line / 高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching parens / 高亮匹配括号
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Highlight brackets according to their depth / 彩虹括号
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords / 高亮 TODO 等关键词
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :init
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb"))))

;; Highlight symbols / 高亮符号
(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-C" . symbol-overlay-remove-all))
  :hook (prog-mode . symbol-overlay-mode)
  :custom (symbol-overlay-idle-time 0.3))

;; Colorize color names in buffers / 在缓冲区中着色颜色名称
(use-package colorful-mode
  :diminish
  :hook (after-init . global-colorful-mode)
  :init (setq colorful-use-prefix t)
  :config
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode)))

;; Highlight indentions / 缩进线
(use-package indent-bars
  :custom
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.4))
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-color-by-depth nil)
  (indent-bars-no-descend-string t)
  (indent-bars-prefer-character t)
  :hook (prog-mode . indent-bars-mode))

;; Pulse modified region / 修改区域脉冲高亮
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode conf-mode) . goggles-mode))

;; Pulse current line / 脉冲当前行
(use-package pulse
  :ensure nil
  :init
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))
  :config
  (dolist (cmd '(recenter-top-bottom
                 other-window
                 switch-to-buffer))
    (advice-add cmd :after #'my-pulse-momentary-line)))

(provide 'init-highlight)

;;; init-highlight.el ends here
