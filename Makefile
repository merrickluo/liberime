UNAME_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

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

EMACS = emacs
VERSION = 1.00
CC = gcc
CFLAGS = -fPIC -O2 -Wall
LDFLAGS = -shared
INCLUDES  = -Isrc
SRC = src
SOURCES = $(wildcard $(SRC)/*.c)
OBJS = $(patsubst %.c, %.o, $(SOURCES))
TARGET = $(SRC)/liberime-core$(SUFFIX)

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
	$(CC) $(OBJS) $(INCLUDES) $(LDFLAGS) $(LIBS) -o $@

test:$(TARGET)
	${EMACS} -Q -L $(SRC) -L . liberime-test.el
