.PHONY: all clean cleanall

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin $<

clean:
	find . -name '*.beam' \
	  -o -name 'erl_crash.dump' \
	  -o -name '*~' \
	  -delete

cleanall: clean
	rm -f ./echessd.log
	rm -rf ./tmp/mnesia

