.PHONY: all doc clean cleanall

COMPILE_FIRST_SOURCES=src/echessd_httpd.erl

ALLSRCS=$(wildcard src/*.erl)
SRCS=$(COMPILE_FIRST_SOURCES) $(filter-out $(COMPILE_FIRST_SOURCES),$(ALLSRCS))
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

ifndef WITHOUT_INETS_HEADER
OPTS=
else
OPTS=-DWITHOUT_INETS_HEADER
endif

all: $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -pa ./ebin -o ./ebin $(OPTS) $<

doc: ebin/echessd.beam
	@echo Making documentation...
	erl -noshell -noinput -pa ./ebin -s echessd build_doc

clean:
	rm -f -- ./doc/*.html
	rm -f -- ./doc/*.css
	rm -f -- ./doc/*.png
	rm -f -- ./doc/edoc-info
	rm -f -- ./ebin/*.beam
	rm -f ./erl_crash.dump
	find ./ -type f -name '*~' -print -delete

cleanall: clean
	rm -f ./echessd.log
	rm -rf ./db/mnesia

