PREFIX ?= $(CURDIR)
UNAME_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
EMACS   := $(shell sh -c 'which emacs')

SUFFIX = .so
LIBRIME = -lrime

## MINGW
ifneq (,$(findstring MINGW,$(UNAME_S)))
	SUFFIX = .dll
	LIBRIME = -llibrime
endif

## macOS
ifneq (,$(findstring Darwin,$(UNAME_S)))
	SUFFIX = .dylib
	CC = clang
	# 使用 pkg-config 获取 brew 安装的 librime 编译参数
	ifneq (,$(shell which pkg-config 2>/dev/null))
		RIME_CFLAGS := $(shell pkg-config --cflags rime 2>/dev/null)
		RIME_LDFLAGS := $(shell pkg-config --libs rime 2>/dev/null)
		ifneq (,$(RIME_CFLAGS))
			CFLAGS += $(RIME_CFLAGS)
			LIBRIME = $(RIME_LDFLAGS)
		endif
	endif
endif

ifdef MODULE_FILE_SUFFIX
	SUFFIX = $(MODULE_FILE_SUFFIX)
endif

VERSION = 1.00
CC = gcc
LDFLAGS += -shared
SRC = src
SOURCES = $(wildcard $(SRC)/*.c)
OBJS = $(patsubst %.c, %.o, $(SOURCES))
TARGET = $(SRC)/liberime-core$(SUFFIX)
CFLAGS += -fPIC -O2 -Wall -DHAVE_RIME_API

ifndef EMACS_MAJOR_VERSION
	EMACS_MAJOR_VERSION := $(shell emacs --batch --eval '(princ emacs-major-version)' 2>/dev/null || echo 26)
endif

CFLAGS += -I emacs-module/$(EMACS_MAJOR_VERSION)
ifdef EMACS_PLUS_PATH
       CFLAGS += -I ${EMACS_PLUS_PATH}
endif

ifdef RIME_PATH
	CFLAGS += -I ${RIME_PATH}/src/
	LDFLAGS += -L ${RIME_PATH}/build/lib/ -L ${RIME_PATH}/build/lib/Release/
	LDFLAGS += -Wl,-rpath,${RIME_PATH}/build/lib/
	LDFLAGS += -Wl,-rpath,${RIME_PATH}/build/lib/Release
	LDFLAGS += $(LIBRIME)
else
	LDFLAGS += $(LIBRIME)
endif


.PHONY:everything objs clean

all:$(TARGET)

objs:$(OBJS)

clean:
	rm -rf $(OBJS) $(TARGET) build test/test_liberime *.elc test/*.elc

$(TARGET):$(OBJS)
	rm -rf build
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $@


# Run integration tests (requires compiled C module + librime)
.PHONY: test-integration
test-integration: $(TARGET)
	emacs --batch -Q -L . -L test \
	  -l ert \
	  -l test/liberime-test.el \
	  -f liberime-test-run

# Run C unit tests (standalone, no Emacs needed)
.PHONY: test-c
test-c: test/test_liberime
	./test/test_liberime

test/test_liberime: test/test_liberime.c
	# $(CC) -o test/test_liberime test/test_liberime.c -Isrc -DHAVE_RIME_API -fPIC -O2 -Wall -lrime
	$(CC) -O2 -Wall -o $@ $<

# Run all tests
.PHONY: test
test: test-c test-integration

liberime-build:
	make -f Makefile-liberime-build

install: ${TARGET}
	install -p -m 755 ${TARGET} $(PREFIX)/lib
