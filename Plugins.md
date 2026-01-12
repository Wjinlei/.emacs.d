# Mint Emacs 插件列表 / Plugins List

本文档记录 Mint Emacs 使用的所有插件及其用法。
This document records all plugins used by Mint Emacs and their usage.

---

## 目录 / Table of Contents

- [主题 / Theme](#主题--theme)

---

## 主题 / Theme

### doom-themes

**描述 / Description:**
一个高质量的 Emacs 主题集合，提供多种精美的暗色和亮色主题。
A collection of high-quality Emacs themes, providing various beautiful dark and light themes.

**仓库 / Repository:** https://github.com/doomemacs/themes

**配置文件 / Config File:** `lisp/init-ui.el`

**可用主题 / Available Themes:**
- `doom-one` - 默认主题，深蓝色调 / Default theme, deep blue
- `doom-one-light` - 亮色版本 / Light version
- `doom-vibrant` - 更鲜艳的配色 / More vibrant colors
- `doom-monokai-pro` - Monokai Pro 风格 / Monokai Pro style
- `doom-dracula` - Dracula 风格 / Dracula style
- `doom-gruvbox` - Gruvbox 风格 / Gruvbox style
- `doom-nord` - Nord 风格 / Nord style
- `doom-palenight` - Palenight 风格 / Palenight style
- `doom-solarized-dark` - Solarized 深色 / Solarized dark
- `doom-solarized-light` - Solarized 亮色 / Solarized light
- `doom-tomorrow-day` - Tomorrow 亮色 / Tomorrow light
- `doom-tomorrow-night` - Tomorrow 深色 / Tomorrow dark
- `doom-zenburn` - Zenburn 风格 / Zenburn style

**命令 / Commands:**

| 命令 / Command | 快捷键 / Keybinding | 说明 / Description |
|----------------|---------------------|---------------------|
| `mint-install-themes` | - | 安装 doom-themes / Install doom-themes |
| `mint-load-theme` | - | 重新加载当前主题 / Reload current theme |
| `mint-switch-theme` | - | 交互式切换主题 / Switch theme interactively |

**配置示例 / Configuration Example:**

在 `custom.el` 中设置主题 / Set theme in `custom.el`:

```elisp
(setq mint-theme 'doom-dracula)
```

---

## 更新日志 / Changelog

- **2026-01-12**: 添加 doom-themes / Added doom-themes
