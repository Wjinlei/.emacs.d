;;; custom-example.el --- Example user customization / 用户自定义示例 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Copy this file to custom.el and modify as needed.
;; 将此文件复制为 custom.el 并根据需要修改。
;;
;; cp custom-example.el custom.el
;;

;;; Code:

;; ==================== Theme / 主题 ====================
;; Available themes / 可用主题:
;; doom-one, doom-one-light, doom-vibrant, doom-monokai-pro,
;; doom-dracula, doom-gruvbox, doom-nord, doom-palenight, etc.
(setq mint-theme 'doom-one)

;; ==================== Font / 字体 ====================
;; Default font / 默认字体
;; Popular choices / 常用字体: "Fira Code", "JetBrains Mono", "Source Code Pro",
;; "Cascadia Code", "Iosevka", "Monaco", "Consolas"
(setq mint-font "Fira Code")

;; Font size / 字体大小
(setq mint-font-size 14)

;;; custom-example.el ends here
