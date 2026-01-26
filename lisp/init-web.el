;;; init-web.el --- Web development configuration / Web开发配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Web development support including Vue, HTML, CSS, JavaScript.
;; Web 开发支持，包括 Vue、HTML、CSS、JavaScript。
;;

;;; Code:

;; Web mode - Major mode for editing web templates / 编辑 Web 模板的主模式
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  :config
  ;; Set Vue file type / 设置 Vue 文件类型
  (add-to-list 'web-mode-content-types-alist '("vue" . "\\.vue\\'"))
  ;; Hooks for web-mode / web-mode 钩子
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set engine for Vue files / 为 Vue 文件设置引擎
              (when (string-equal "vue" (file-name-extension buffer-file-name))
                (setq-local web-mode-enable-auto-quoting nil)))))

;; CSS mode / CSS 模式
(use-package css-mode
  :ensure nil
  :init
  (setq css-indent-offset 2))

;; SCSS mode / SCSS 模式
(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

;; JavaScript / JavaScript 配置
(use-package js
  :ensure nil
  :init
  (setq js-indent-level 2))

;; TypeScript / TypeScript 配置
(use-package typescript-mode
  :mode "\\.ts\\'"
  :init
  (setq typescript-indent-level 2))

;; JSON mode / JSON 模式
(use-package json-mode
  :mode "\\.json\\'")

;; Emmet for fast HTML/CSS editing / 快速 HTML/CSS 编辑
(use-package emmet-mode
  :hook ((web-mode css-mode scss-mode) . emmet-mode)
  :init
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t))

;; Add node_modules/.bin to exec-path / 添加 node_modules/.bin 到路径
(use-package add-node-modules-path
  :hook ((web-mode js-mode typescript-mode) . add-node-modules-path))

(provide 'init-web)

;;; init-web.el ends here
