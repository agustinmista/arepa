ifeq ($(DEBUG),1)
   EXTRA_OPTS = -g -DDEBUG
endif

CC ?= clang
CFLAGS ?= -Wall -Wextra -Werror -O2 -Iinclude $(EXTRA_OPTS)

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