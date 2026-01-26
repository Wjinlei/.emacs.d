;;; init-completion.el --- Completion configuration / 补全配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modern completion with Consult, Vertico, Marginalia.
;; 使用 Consult, Vertico, Marginalia 的现代补全系统。
;;

;;; Code:

;; Optionally use the `orderless' completion style
;; 使用 orderless 补全风格
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; VERTical Interactive COmpletion
;; 垂直交互式补全
(use-package vertico
  :ensure t
  :custom (vertico-count 15)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Enrich existing commands with completion annotations
;; 为补全候选添加注释
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; Add icons to completion candidates
;; 为补全候选添加图标
(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Consulting completing-read
;; Consult - 增强的补全读取
(use-package consult
  :ensure t
  :bind (;; Yank / 粘贴
         ("M-y"     . consult-yank-pop)

         ;; Remap bindings / 重映射绑定
         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Register formatting / 寄存器格式化
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  ;; 使用 Consult 选择 xref 位置并预览
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  ;; Preview key configuration / 预览键配置
  (setq consult-preview-key nil)
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep :preview-key '(:debounce 0.5 any))

  ;; Narrowing key / 缩小范围键
  (setq consult-narrow-key "<"))

;; Directory navigation for consult
;; Consult 目录导航
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Auto completion with Corfu / 使用 Corfu 自动补全
(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling / 启用循环（到底/顶时跳转）
  (corfu-auto t)                 ; Enable auto completion / 启用自动补全
  (corfu-auto-prefix 2)          ; Minimum prefix length / 最小前缀长度
  (corfu-auto-delay 0.2)         ; Delay before showing / 显示延迟
  (corfu-count 12)               ; Number of candidates / 候选数量
  (corfu-preview-current nil)    ; Don't preview current / 不预览当前
  (corfu-on-exact-match nil)     ; Don't auto-insert on exact match / 精确匹配不自动插入
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (("M-/" . completion-at-point))  ; Manual trigger / 手动触发
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         (global-corfu-mode . corfu-history-mode))
  :config
  ;; Keybindings in corfu-map / 补全菜单中的快捷键
  (keymap-set corfu-map "<down>" #'corfu-next)
  (keymap-set corfu-map "<up>" #'corfu-previous)
  (keymap-set corfu-map "C-j" #'corfu-next)
  (keymap-set corfu-map "C-k" #'corfu-previous)
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous)
  (keymap-set corfu-map "TAB" #'corfu-insert)
  (keymap-set corfu-map "RET" #'corfu-insert)
  (keymap-set corfu-map "C-g" #'corfu-quit)
  ;; Quit completion before saving / 保存前退出补全
  (add-hook 'before-save-hook #'corfu-quit))

;; Corfu icons / Corfu 图标
(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape - Completion At Point Extensions / 补全扩展
(use-package cape
  :init
  ;; Add useful completion sources / 添加补全源
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  ;; Make eglot capf composable / 使 eglot 补全可组合
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

;; Dabbrev configuration / Dabbrev 配置
(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; TAB configuration / TAB 配置
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)  ; TAB indents or completes / TAB 缩进或补全
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'init-completion)

;;; init-completion.el ends here
