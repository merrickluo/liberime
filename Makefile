CC       = gcc
EMACS    = emacs
MINGW_CC = x86_64-w64-mingw32-gcc
CFLAGS   = -ggdb3 -Wall
LDFLAGS =

MODULE_SUFFIX := $(shell $(EMACS) -batch --eval '(princ module-file-suffix)')

all : librime-emacs.so librime-emacs.dll librime.elc
linux : librime-emacs.so librime.elc
windows : librime-emacs.dll librime.elc

librime-emacs.so : librime-emacs.c
	$(CC) -shared $(CFLAGS) -o $@ $^

librime-emacs.dll : librime-emacs.c
	$(MINGW_CC) -shared $(CFLAGS) -o $@ $^

librime.elc : librime.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

run : librime.elc librime-emacs$(MODULE_SUFFIX)
	$(EMACS) -Q -L . -l $< -f librime

test: emacs-rime.c
	$(CC) -g  -o test $^ -lrime && ./test

clean :
	$(RM) librime-emacs.so librime-emacs.dll librime.elc test

.PHONY : clean all linux windows
