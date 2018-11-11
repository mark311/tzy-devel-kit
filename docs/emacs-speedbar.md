# Emacs Speedbar

在GUI的模式下，Emacs Speedbar帮助快速定位文件，类似于一个文件浏览器，
同时也提供Buffer列表的功能，它能极大提高工作效率。

有两种speedbar：
1. speedbar - 内置的、普通的speedbar
2. sr-speedbar - 在speedbar的基础上，将speedbar frame 和 main frame合并

总体上，两种speedbar各有缺陷，都不是很完美，但我最终还是选择了内置的
speedbar。原因是，内置的speedbar由于采用单独的frame，它对main frame的
各种操作带来的影响是最小的。而sr-speedbar将speedbar frame融合到main
frame中，两者之间会有一些相互影响，比如other-frame等，虽然有绕过的办法，
但目前看来并不彻底，容易引入奇葩的问题。

## Speedbar

* `M-x speedbar` 能开启或关闭 speedbar frame。
* `M-x speedbar-get-focus` 可用来激活speedbar frame，也可用来开启
  speedbar frame

### 缺陷

speedbar有个问题，由于它在GUI模式下是新启了一个窗口，这使得通过
Speedbar窗口和主窗口不能同时被激活，例如Emacs在被其他窗口遮挡的情况下，
点击Emacs主窗口的边角只能激活主窗口本身，不能同时激活Speedbar窗口。为
了避免窗口分离带来的这个问题，有两个办法：

1. `M-x speedbar-get-focus` 可用来激活speedbar frame

2. 我们引进`sr-speedbar`，它能将Speedbar放置与Emacs主窗口的右侧，避免
创建单独的窗口。


## Sr-Speedbar

sr-speedbar官方文档这里：https://www.emacswiki.org/emacs/SrSpeedbar

### 安装

melpa上有sr-speedbar的包，直接用`list-packages`找到`sr-speedbar`并安装。
melpa会自动加载包，所以不需要在~/.emacs文件中显式require了。

### 用法

我的使用习惯是让speedbar常驻（或常关闭），不存在临时打开用一下就关闭的
场景，这样的好处是不需要为sr-speedbar分配全局快捷键，这是很宝贵的资源！

* `M-x sr-speedbar-open` 打开sr-speedbar（内嵌的）窗口
* `M-x sr-speedbar-toggle` 打开、关闭sr-speedbar窗口


如果第一次打开的sr-speedbar仅仅是复制了当前buffer，那么你需要把普通的
speedbar窗口关闭掉。

    (If it simply duplicates the current buffer the first time you open it, make sure you’ve closed normal SpeedBar’s frame first.)

### sr-speedbar的独立性

我们需要让sr-speedbar静静地待在main frame的侧边，不要对main frame产生
影响，就像它待在独立的frame中一样。

1. 当你使用`C-x 1`关闭其他窗口的时候，sr-speedbar的窗口会保留。

2. 不会被cycle select到，（*这个目前没有配置成功过*）

   * `(setq sr-speedbar-skip-other-window-p t)`
