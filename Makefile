UNAME_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
EMACS   := $(shell sh -c 'which emacs')

SUFFIX = .so
LIBRIME = -lrime

## MINGW
ifneq (,$(findstring MINGW,$(UNAME_S)))
	SUFFIX = .dll
	LIBRIME = -llibrime
endif

ifdef MODULE_FILE_SUFFIX
	SUFFIX = $(MODULE_FILE_SUFFIX)
endif

VERSION = 1.00
CC = gcc
LDFLAGS = -shared
SRC = src
SOURCES = $(wildcard $(SRC)/*.c)
OBJS = $(patsubst %.c, %.o, $(SOURCES))
TARGET = $(SRC)/liberime-core$(SUFFIX)
CFLAGS = -fPIC -O2 -Wall

ifndef EMACS_MAJOR_VERSION
	EMACS_MAJOR_VERSION = 26
endif

ifndef EMACS
	CFLAGS += -I emacs-module/$(EMACS_MAJOR_VERSION)
endif

ifdef RIME_PATH
	CFLAGS += -I ${RIME_PATH}/src/
	LDFLAGS += -L ${RIME_PATH}/build/lib/ -L ${RIME_PATH}/build/lib/Release/
	LDFLAGS += -Wl,-rpath,${RIME_PATH}/build/lib/:${RIME_PATH}/build/lib/Release $(LIBRIME)
else
	LDFLAGS += $(LIBRIME)
endif


.PHONY:everything objs clean

all:$(TARGET)

objs:$(OBJS)

clean:
	rm -rf $(OBJS) $(TARGET) build

$(TARGET):$(OBJS)
	rm -rf build
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $@

test:$(TARGET)
	${EMACS} -Q -L $(SRC) -L . liberime-test.el

liberime-build:
	make -f Makefile-liberime-build
