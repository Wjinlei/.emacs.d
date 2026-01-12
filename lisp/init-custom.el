;;; init-custom.el --- Define customizations / 自定义变量定义 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customization variables for user configuration.
;; 用户可配置的自定义变量。
;;

;;; Code:

;; Custom group / 自定义组
(defgroup mint nil
  "Mint Emacs customization. / Mint Emacs 自定义配置组。"
  :group 'convenience)

;; Theme setting / 主题设置
(defcustom mint-theme 'doom-one
  "The color theme to use. / 使用的颜色主题。
Available themes / 可用主题: doom-one, doom-one-light, doom-vibrant,
doom-monokai-pro, doom-dracula, doom-gruvbox, doom-nord, etc."
  :group 'mint
  :type 'symbol)

;; Font settings / 字体设置
(defcustom mint-font "Fira Code"
  "The default font family. / 默认字体。"
  :group 'mint
  :type 'string)

(defcustom mint-font-size 14
  "The default font size. / 默认字体大小。"
  :group 'mint
  :type 'integer)

;; Custom file path / 自定义文件路径
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;; init-custom.el ends here
