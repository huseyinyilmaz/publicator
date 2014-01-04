DIALYZER = dialyzer
REBAR = ./rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = publicator

build: clean compile
	@$(REBAR) generate
	cp -r www rel/publicator
	cd rel; tar -czvf publicator.tar.gz publicator
	rm -rf rel/publicator
	mv rel/publicator.tar.gz .
	echo "publicator.tar.gz is created in current directory."

build2: clean compile
	rm -rf rel/publicator
	@$(REBAR) generate
	rel/publicator/bin/publicator start
	sleep 2
	cat rel/publicator/log/erlang*
# compile
all: compile

# get dependencies
get-deps:
	@$(REBAR) get-deps

# initialize development
configure-dev:
	cd devutils; if [ ! -d "sync" ]; then git clone git://github.com/rustyio/sync.git sync; fi
	cd devutils/sync; mkdir -p ebin; make

configure: get-deps

compile:
	@$(REBAR) compile
	cd devutils; if [ -d "sync" ]; then cd sync;make; fi
	cd devutils; if [ -d "utils" ]; then cd utils;mkdir -p ebin;make; fi

clean:
	@$(REBAR) clean
	if [ -d "test" ]; then rm -f test/*.beam; fi
	rm -f erl_crash.dump

test: clean compile eunit

eunit:
	@$(REBAR) eunit skip_deps=true

ct:

	@$(REBAR) ct skip_deps=true

build-erlang-plt:
	@$(DIALYZER) --build_plt --output_plt .erlang_dialyzer.plt \
		--apps erts kernel stdlib ssl crypto

build-plt:
	@$(DIALYZER) --build_plt --add_to_plt --plt .erlang_dialyzer.plt \
		--output_plt .publicator_dialyzer.plt \
		--apps deps/*

dialyze:
	@$(DIALYZER) --src lib/*/src --plt .publicator_dialyzer.plt\
		-Werror_handling \
		-Wrace_conditions -Wunmatched_returns \
		| grep -v -f ./.dialyzer-ignore-warnings
# -Wunderspecs
docs:
	@$(REBAR) doc skip_deps=true

# start for development
start: compile
	erl -pa lib/*/ebin deps/*/ebin devutils/*/ebin \
	    -i  lib/*/include deps/*/include devutils/*/include \
	    -boot start_sasl \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME) \
	    -eval "application:start(gproc),sync:go()." \
	    -s lager \
	    -s server \
	    -s http\


blackbox:
	rm -f publicator_test
	cd devutils/publicator_test/; ../../rebar compile escriptize;mv publicator_test ../../
	./publicator_test
