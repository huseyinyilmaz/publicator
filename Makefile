DIALYZER = dialyzer
REBAR = ./rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = publicator

# compile
all: compile

# get dependencies
get-deps:
	@$(REBAR) get-deps

# initialize development
init-dev:
	cd devutils; if [ ! -d "sync" ]; then git clone git://github.com/rustyio/sync.git sync; fi
	cd devutils/sync; mkdir -p ebin; make

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

pack: init compile
	@$(REBAR) generate
	cp -r www rel/talkybee
	cd rel; tar -czvf talkybee.tar.gz talkybee
	rm -rf rel/talkybee
	mv rel/talkybee.tar.gz .
	echo "talkybee.tar.gz is created in current directory."
