假设 emacs.exe 所在的目录是 D:/MYDIR/bin, 那么：

1. 确保 (executable-find "emacs") 可以返回 emacs 的正确路径，
   如果不行，请正确设置 PATH 环境变量。
2. 将 liberime-archive.zip 解压缩到 D:/MYDIR, 确保 librime.dll
   与 emacs.exe 在同一目录。
5. 第一次使用 liberime 的时候，会 deploy 词库，速度比较慢，
   会导致 emacs 卡住一段时间，要耐性等待。

