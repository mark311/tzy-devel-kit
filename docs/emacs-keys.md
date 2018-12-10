# Emacs Keys

## 搜索

* `M-s M-s` 或 `M-s s` - 搜索当前位置单词

## 括号操作

* `M-(`     - 插入一对括号。如果有被选择区域，则把选中区域包围。
* `C-u M-(` - 删除当前括号对，调用`delete-pair`。  （这个是我自己overwrite的行为）
* `C-M-k`   - 删除括号及内容
* `C-M-u`   - 移动到上级括号
* `C-M-n`   - 移动下一个平级括号
* `C-M-p`   - 移动上一个平级括号

## C-c <letter> 系列

在[D.2 Key Binding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html)中有关于`C-c <letter>`的按键约定，如下：

    Don't define C-c letter as a key in Lisp programs. Sequences consisting of C-c and a letter (either upper or lower case) are reserved for users; they are the only sequences reserved for users, so do not block them.
    Changing all the Emacs major modes to respect this convention was a lot of work; abandoning this convention would make that work go to waste, and inconvenience users. Please comply with it.

作为一个用户，我有权进行重定义 :)


* `C-c w g` 激活magit的主界面
* `C-c p c` 调用recompile进行项目编译构建
* `C-c w w` 激活speedbar。右手一般预先已经挪到了鼠标或触摸板上，激活的操作需要左手单手快速完成。
* `C-c f r` revert-buffer
* `C-c f w` delete white spaces

## GUI模式快速关闭窗口

仍然使用 `C-x 1` 想关闭其他窗口

由于 `C-x 0` 需要两手操作（GUI模式下右手可能放在鼠标或触控板上），并且
关闭之前需要先切换到目标窗口，这些都降低了关闭指定窗口的效率。我们考虑
使用Mode Line的右击([mode-line mouse-3])来关闭当前窗口，具体操作是，将
鼠标放到Mode Line的空白位置，右击（产生mouse-3而不是mouse-2），你会观
察到窗口被关闭掉了。

参考: [Mode Line Mouse Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line-Mouse.html#Mode-Line-Mouse)

## 高亮

* `M-s h h` 或 `M-s h .` - 使当前位置单词高亮
* `M-s h u` - 取消单词高亮。

## Magit (git plugin)

* `M-x magit` 或 `M-x magit-status` 打开magit的status buffer，所有git操作都可以
交互式地在status buffer中进行，无须记忆其他命令或快捷键。在status buffer中，几乎
任何时候都可以按`h`调出帮助文档，非常方便。

## TAGS

**生成TAGS文件**

有两种生成TAGS文件的方式：
* 一是生成全局的TAGS
* 二是per directory生成局部的TAGS，然后分别加载这些TAGS

第一种方式生成，进入代码的根目录（就是希望TAGS生成的目标目录），执行

`M-! find . -type f -iname "*.[ch]" -o -iname "*.cpp" | etags -`

第二种方式生成：

`M-! etags *.go`

然后加载它，

`M-x visit-tags-table RET ... RET`

清空已经加载的tags table:

`M-x tags-reset-tags-tables RET`

**查找符号**

`M-.` 查找符号

`M-,` 回退至上个位置

## Org Table

* `C-c C-t` - 切换完成状态TODO, DONE
* `C-c .` - 添加时间戳
* `C-c ,` - 改变优先级（注意这是interactive的）
* `TAB` or `C-i` - 收起/展开当前项目（连续一次是展开当前level，连续两次是展开全部levels）

## Book Marks

* `C-x r m` - set a bookmark at the current location (e.g. in a file)
* `C-x r b` - jump to a bookmark
* `C-x r l` - list your bookmarks
* `M-x bookmark-delete` - delete a bookmark by name

## Speedbar

`M-x speedbar` 启动Speedbar

在speedbar激活的状态下：

`b` 转到Quick Buffer的显示模式

`f` 转到Files的显示模式
