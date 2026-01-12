;;; early-init.el --- Early initialization / 早期初始化 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27+ loads this file before init.el, before package and UI initialization.
;; Emacs 27+ 会在 init.el 之前加载此文件，在包和 UI 初始化之前执行。
;;

;;; Code:

;; Defer garbage collection further back in the startup process
;; 延迟垃圾回收以加速启动
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for native-comp users
;; 禁用原生编译的即时编译，避免启动时编译
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; 包管理会在 early-init 之后自动初始化，我们需要阻止它提前执行
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
;; 禁止自动调整窗口大小
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
;; 在初始化之前禁用 UI 元素（更快）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent flash of unstyled mode line at startup
;; 防止启动时模式栏闪烁
(setq-default mode-line-format nil)

;;; early-init.el ends here
