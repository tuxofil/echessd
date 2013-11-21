APP = echessd

VERSION = $(shell cat version)

.PHONY: all compile doc clean eunit dialyze all-tests \
	install-doc install-html debian-install debian-uninstall

all: $(APP)

COPTS = {outdir, ebin}, {i, \"include\"}, warn_unused_function, \
 warn_bif_clash, warn_deprecated_function, warn_obsolete_guard, verbose, \
 warn_shadow_vars, warn_export_vars, warn_unused_records, \
 warn_unused_import, warn_export_all, warnings_as_errors

ifdef DEBUG
COPTS := $(COPTS), debug_info
endif

ifdef TEST
COPTS := $(COPTS), {d, 'TEST'}
endif

ifdef TRACE
COPTS := $(COPTS), {d, 'TRACE'}
endif

OTPREL = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)),halt()')
ifeq ($(shell expr $(OTPREL) '<' R14B02), 1)
COPTS := $(COPTS), {d, 'WITHOUT_INETS_HEADER'}
endif

FIRST_MODS = "src/echessd_httpd",

compile:
	mkdir -p ebin
	sed "s/{{VERSION}}/$(VERSION)/" src/$(APP).app.in > ebin/$(APP).app
	echo '[$(FIRST_MODS)"src/*"].' > Emakefile
	erl -pa ebin -noinput -eval "up_to_date=make:all([$(COPTS)]),halt()"

$(APP): compile
	rm -f -- $(APP).zip
	zip -j $(APP) ebin/*
	zip $(APP) priv/$(APP).lang priv/$(APP).styles priv/www/*
	echo '#!/usr/bin/env escript' > $(APP)
	echo '%%!-smp' >> $(APP)
	cat $(APP).zip >> $(APP)
	rm -f -- $(APP).zip
	chmod 755 $(APP)

html:
	sed "s/{{VERSION}}/$(VERSION)/" doc/overview.edoc.in > doc/overview.edoc
	erl -noinput -eval \
		'edoc:application($(APP),".",[{application,$(APP)}]),halt()'

eunit:
	$(MAKE) TEST=y clean compile
	erl -noinput -pa ebin \
		-eval 'ok=eunit:test({application,$(APP)},[verbose]),halt()'

PLT = .dialyzer_plt
DIALYZER_OPTS = -Wunmatched_returns -Werror_handling -Wrace_conditions

dialyze: $(PLT)
	dialyzer --plt $< -r . $(DIALYZER_OPTS) --src
	$(MAKE) DEBUG=y clean compile
	dialyzer --plt $< -r . $(DIALYZER_OPTS)

$(PLT):
	dialyzer --build_plt --output_plt $@ \
		--apps erts inets kernel stdlib crypto compiler

all-tests:
	$(MAKE) eunit
	$(MAKE) dialyze

clean:
	rm -rf -- ebin doc/*.html doc/*.css doc/*.png doc/edoc-info \
	    $(APP).zip $(APP) erl_crash.dump Emakefile doc/overview.edoc \
	    *.log *.log.* tmp_file
	find . -type f -name '*~' -delete

