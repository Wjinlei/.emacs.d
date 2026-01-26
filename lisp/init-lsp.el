;;; init-lsp.el --- LSP configuration with Eglot / LSP配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Language Server Protocol configuration using Eglot.
;; 使用 Eglot 的语言服务器协议配置。
;;

;;; Code:

;; Eglot - Built-in LSP client / 内置 LSP 客户端
(use-package eglot
  :ensure nil
  :hook ((go-mode go-ts-mode) . eglot-ensure)
  :hook ((vue-mode web-mode) . eglot-ensure)
  :hook ((js-mode js-ts-mode typescript-mode typescript-ts-mode) . eglot-ensure)
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  ;; Vue/Volar support / Vue/Volar 支持
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((web-mode :language-id "vue") . ("vue-language-server" "--stdio")))

  ;; Enable snippet expansion for function completion / 启用函数补全时的代码片段展开
  ;; This adds () after function names automatically / 自动在函数名后添加括号
  (setq eglot-ignored-server-capabilities nil)  ; Don't ignore any capabilities
  
  ;; Hook to enable yasnippet for eglot / 为 eglot 启用 yasnippet
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (fboundp 'yas-minor-mode)
                (yas-minor-mode 1)))))

;; Consult-eglot - Consult integration for Eglot / Eglot 的 Consult 集成
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
         ("C-M-." . consult-eglot-symbols)))

(provide 'init-lsp)

;;; init-lsp.el ends here
