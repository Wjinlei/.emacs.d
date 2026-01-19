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
  :bind (;; C-c bindings / C-c 绑定
         ("C-c i"   . consult-imenu)

         ;; C-x bindings / C-x 绑定
         ("C-x b"   . consult-buffer)

         ;; Yank / 粘贴
         ("M-y"     . consult-yank-pop)

         ;; M-s bindings (search-map) / M-s 绑定（搜索）
         ("M-s l"   . consult-line)

         ;; Remap bindings / 重映射绑定
         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

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

(provide 'init-completion)

;;; init-completion.el ends here
