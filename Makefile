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
initdev: init
	cd devutils; if [ ! -d "sync" ]; then git clone git://github.com/rustyio/sync.git sync; fi
	cd devutils/sync; mkdir -p ebin; make

compile:
	@$(REBAR) compile
	cd devutils; if [ -d "sync" ]; then cd sync;make; fi
	cd devutils; if [ -d "utils" ]; then cd utils;mkdir -p ebin;make; fi

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

test: clean eunit

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .publicator_dialyzer.plt \
		--apps kernel stdlib deps/*

dialyze:
	@$(DIALYZER) --src lib/*/src --plt .publicator_dialyzer.plt -Werror_handling \
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
