# Mint Emacs 插件列表 / Plugins List

本文档记录 Mint Emacs 使用的所有插件及其用法。
This document records all plugins used by Mint Emacs and their usage.

---

## 目录 / Table of Contents

- [主题 / Theme](#主题--theme)
- [Vim 模拟 / Vim Emulation](#vim-模拟--vim-emulation)
- [版本控制 / Version Control](#版本控制--version-control)

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

---

## Vim 模拟 / Vim Emulation

### evil

**描述 / Description:**
在 Emacs 中提供 Vim 风格的模态编辑，是最完整的 Vim 模拟层。
Provides Vim-style modal editing in Emacs, the most complete Vim emulation layer.

**仓库 / Repository:** https://github.com/emacs-evil/evil

**配置文件 / Config File:** `lisp/init-evil.el`

**模式说明 / Mode Description:**

| 模式 / Mode | 说明 / Description |
|-------------|---------------------|
| Normal | 普通模式，用于导航和命令 / Normal mode for navigation and commands |
| Insert | 插入模式，用于输入文本 / Insert mode for typing text |
| Visual | 可视模式，用于选择文本 / Visual mode for selecting text |
| Emacs | Emacs 模式，使用原生键位 / Emacs mode with native keybindings |

---

### evil-collection

**描述 / Description:**
为 Evil 提供额外的键绑定集合，覆盖更多 Emacs 内置模式和第三方包。
Provides additional keybindings for Evil, covering more built-in modes and third-party packages.

**仓库 / Repository:** https://github.com/emacs-evil/evil-collection

**配置文件 / Config File:** `lisp/init-evil.el`

---

### evil-escape

**描述 / Description:**
使用快捷键序列（如 jk）退出插入模式，避免按 ESC 键。
Use key sequence (like jk) to escape from insert mode, avoiding pressing ESC.

**仓库 / Repository:** https://github.com/syl20bnr/evil-escape

**配置文件 / Config File:** `lisp/init-evil.el`

**默认配置 / Default Config:**
- 退出序列 / Escape sequence: `jj`
- 延迟 / Delay: `0.2` 秒

---

---

## 版本控制 / Version Control

### magit

**描述 / Description:**
Emacs 中最强大的 Git 界面，提供完整的 Git 操作支持。
The most powerful Git interface in Emacs, providing complete Git operation support.

**仓库 / Repository:** https://github.com/magit/magit

**配置文件 / Config File:** `lisp/init-vcs.el`

**快捷键 / Keybindings:**

| 快捷键 / Key | 命令 / Command | 说明 / Description |
|--------------|----------------|---------------------|
| `C-x g` | `magit-status` | 打开 Git 状态 / Open Git status |
| `C-x M-g` | `magit-dispatch` | Git 命令菜单 / Git command menu |
| `C-c M-g` | `magit-file-dispatch` | 当前文件 Git 操作 / Git operations for current file |

**常用操作 / Common Operations (in magit-status):**

| 按键 / Key | 说明 / Description |
|------------|---------------------|
| `s` | Stage 文件/区块 / Stage file/hunk |
| `u` | Unstage 文件/区块 / Unstage file/hunk |
| `c c` | 提交 / Commit |
| `P p` | 推送 / Push |
| `F p` | 拉取 / Pull |
| `b b` | 切换分支 / Switch branch |
| `b c` | 创建分支 / Create branch |
| `l l` | 查看日志 / View log |
| `d d` | 查看差异 / View diff |
| `q` | 退出 / Quit |

---

### diff-hl

**描述 / Description:**
在边栏显示 Git 更改状态（新增、修改、删除）。
Show Git change status (added, modified, deleted) in the gutter.

**仓库 / Repository:** https://github.com/dgutov/diff-hl

**配置文件 / Config File:** `lisp/init-vcs.el`

**标记说明 / Markers:**
- 绿色 / Green: 新增行 / Added lines
- 紫色 / Purple: 修改行 / Modified lines
- 红色 / Red: 删除行 / Deleted lines

---

## 更新日志 / Changelog

- **2026-01-12**: 添加 magit, diff-hl / Added magit, diff-hl
- **2026-01-12**: 添加 evil, evil-collection, evil-escape / Added evil, evil-collection, evil-escape
- **2026-01-12**: 添加 doom-themes / Added doom-themes
