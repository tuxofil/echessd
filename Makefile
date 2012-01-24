.PHONY: all clean cleanall

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin $<

clean:
	rm -f -- ./ebin/*.beam
	rm -f ./erl_crash.dump
	find ./ -type f -name '*~' -print -delete

cleanall: clean
	rm -f ./echessd.log
	rm -rf ./tmp/mnesia

