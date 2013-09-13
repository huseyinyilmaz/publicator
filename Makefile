DIALYZER = dialyzer
REBAR = ./rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = publicator

pack: compile
	@$(REBAR) generate
	cd rel; tar -czvf publicator.tar.gz publicator
	rm -rf rel/publicator
	mv rel/publicator.tar.gz .
	echo "publicator.tar.gz is created in current directory."

# compile
all: compile

# get dependencies
get-deps:
	@$(REBAR) get-deps

# initialize development
configure-dev:
	cd devutils; if [ ! -d "sync" ]; then git clone git://github.com/rustyio/sync.git sync; fi
	cd devutils/sync; mkdir -p ebin; make

configure: get-deps collectstatic

collectstatic:
	cp deps/bullet/priv/bullet.js www/js/bullet.js

compile:
	@$(REBAR) compile
	cd devutils; if [ -d "sync" ]; then cd sync;make; fi
	cd devutils; if [ -d "utils" ]; then cd utils;mkdir -p ebin;make; fi

clean:
	@$(REBAR) clean
	if [ -d "test" ]; then rm -f test/*.beam; fi
	rm -f erl_crash.dump

test: clean compile eunit ct

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
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

# start for development
start: compile
	erl -pa lib/*/ebin deps/*/ebin devutils/*/ebin \
	    -i  lib/*/include deps/*/include devutils/*/include \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME) \
	    -eval "debug:init(),\
	           debug:start()."

