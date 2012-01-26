.PHONY: all doc clean cleanall

SRCS=$(wildcard src/*.erl)
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: $(BEAMS) doc

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -o ./ebin $<

doc: ebin/echessd.beam
	@echo Making documentation...
	mkdir -p ./doc
	erl -noshell -noinput -pa ./ebin -s echessd build_doc

clean:
	rm -f -- ./doc/*
	rm -f -- ./ebin/*.beam
	rm -f ./erl_crash.dump
	find ./ -type f -name '*~' -print -delete

cleanall: clean
	rm -f ./echessd.log
	rm -rf ./db/mnesia

