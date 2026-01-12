;;; init-ui.el --- UI and theme configuration / UI与主题配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Theme installation and UI configuration.
;; 主题安装与 UI 配置。
;;

;;; Code:

(require 'init-custom)

;; Font configuration / 字体配置
(defun mint-setup-fonts ()
  "Setup fonts. / 设置字体。"
  (interactive)
  (when (display-graphic-p)
    ;; Default font / 默认字体
    (when (member mint-font (font-family-list))
      (set-face-attribute 'default nil
                          :family mint-font
                          :height (* mint-font-size 10)))))

;; Setup fonts on frame creation / 创建窗口时设置字体
(add-hook 'after-init-hook #'mint-setup-fonts)
(add-hook 'server-after-make-frame-hook #'mint-setup-fonts)

;; Theme installation function / 主题安装函数
(defun mint-install-themes ()
  "Install doom-themes package if not already installed.
安装 doom-themes 包（如果尚未安装）。"
  (interactive)
  (unless (package-installed-p 'doom-themes)
    (package-refresh-contents)
    (package-install 'doom-themes))
  (message "doom-themes installed successfully! / doom-themes 安装成功！"))

;; Load theme function / 加载主题函数
(defun mint-load-theme ()
  "Load the theme specified in `mint-theme'.
加载 `mint-theme' 指定的主题。"
  (interactive)
  ;; Ensure doom-themes is installed / 确保 doom-themes 已安装
  (unless (package-installed-p 'doom-themes)
    (mint-install-themes))
  ;; Disable all existing themes first / 先禁用所有已启用的主题
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the specified theme / 加载指定的主题
  (load-theme mint-theme t)
  (message "Loaded theme: %s / 已加载主题: %s" mint-theme mint-theme))

;; Switch theme interactively / 交互式切换主题
(defun mint-switch-theme ()
  "Switch to a different doom theme interactively.
交互式切换到其他 doom 主题。"
  (interactive)
  (let ((theme (intern
                (completing-read
                 "Select theme / 选择主题: "
                 '(doom-one doom-one-light doom-vibrant
                   doom-monokai-pro doom-dracula doom-gruvbox
                   doom-nord doom-palenight doom-solarized-dark
                   doom-solarized-light doom-tomorrow-day
                   doom-tomorrow-night doom-zenburn)))))
    (setq mint-theme theme)
    (mint-load-theme)))

;; Use doom-themes / 使用 doom-themes
(use-package doom-themes
  :ensure t
  :demand t  ; Force immediate loading / 强制立即加载
  :config
  ;; Global settings (defaults) / 全局设置（默认值）
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Load the theme / 加载主题
  (load-theme mint-theme t)
  ;; Enable flashing mode-line on errors / 错误时闪烁模式栏
  (doom-themes-visual-bell-config))

(provide 'init-ui)

;;; init-ui.el ends here
