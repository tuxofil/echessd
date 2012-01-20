.PHONY: all clean

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: $(BEAMS)

ebin/%.beam: src/%.erl
	erlc -I include -o ebin $<

clean:
	rm -f -- ebin/*.beam
	rm -rf Mnesia.*
	rm -f erl_crash.dump
	rm -f ./*~

