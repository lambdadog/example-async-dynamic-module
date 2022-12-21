# What a horrible night to have a curse
#
# Most emacs dynamic modules vendor emacs_module.h into their repos
# but I feel this is a poor decision... Of course, emacs doesn't make
# this any easier by not including a pkg-config configuration for it
EMACS_MODULE_INCLUDE=$(shell echo $$(realpath $$(dirname $$(realpath $$(which emacs)))/../include/))

CFLAGS=-Wall -Wextra -pedantic -fPIC -I$(EMACS_MODULE_INCLUDE)
LDFLAGS=-lpthread

example-async-dynamic-module-dyn.so: example-async-dynamic-module-dyn.o
	$(CC) -shared $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) *.so *.o
