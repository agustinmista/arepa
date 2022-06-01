ifeq ($(DEBUG),1)
   EXTRA_OPTS = -g -DDEBUG -O0
endif

ifeq ($(NO_GC),1)
   GC_OPTS = -DNO_GC
endif

CC ?= clang
CFLAGS ?= -Wall -Wextra -Werror -O2 -Iinclude $(EXTRA_OPTS) $(GC_OPTS)

LIB_SRCS = $(wildcard src/*.c)
LIB_OBJS = $(LIB_SRCS:.c=.o)

TEST_SRCS = $(wildcard test/*.c)
TEST_EXES = $(patsubst %.c,%,$(TEST_SRCS))

.PHONY: all
all: ${LIB_OBJS} ${TEST_EXES}

$(LIB_SRCS:.c=.d):%.d:%.c
	$(CC) $(CFLAGS) -MM $< >$@

%: %.c ${LIB_OBJS}
	$(CC) $(CFLAGS) -o $@ $< ${LIB_OBJS} $(LDFLAGS)

.PHONY: clean
clean:
	rm -f ${LIB_OBJS} ${TEST_EXES}