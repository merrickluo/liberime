假设 emacs.exe 所在的目录是 D:/MYDIR/bin, 那么：

1. 将 liberime-archive.zip 解压缩到 D:/MYDIR。
2. 确保 (locate-file "librime" exec-path '(".dll")) 可以返回
   librime.dll 的路径，如果不行，请手动设置 PATH 环境变量。
3. 第一次使用 liberime 的时候，会 deploy 词库，速度比较慢，
   会导致 emacs 卡住一段时间，要耐性等待。

