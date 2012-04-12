.PHONY: all doc clean cleanall erlc_opts

COMPILE_FIRST_SOURCES=src/echessd_httpd.erl

ALLSRCS=$(wildcard src/*.erl)
SRCS=$(COMPILE_FIRST_SOURCES) $(filter-out $(COMPILE_FIRST_SOURCES),$(ALLSRCS))
BEAMS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: erlc_opts $(BEAMS)

ebin/%.beam: src/%.erl include/*.hrl
	erlc -I ./include -pa ./ebin -o ./ebin `cat erlc_opts` $<

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
	rm -f ./erlc_opts
	rm -f ./otp_release
	rm -f ./echessd.log
	rm -rf ./db/mnesia

.ONESHELL:
erlc_opts:
	erl -noshell -noinput \
		-eval 'io:format("~s~n", [erlang:system_info(otp_release)])' \
		-s init stop > otp_release
	expr `cat otp_release` '<' R14B02 > /dev/null && \
		echo "-DWITHOUT_INETS_HEADER" > erlc_opts || :

