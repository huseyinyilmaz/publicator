DIALYZER = dialyzer
REBAR = ./rebar
RELX = ./relx
MNESIA_DIR = /tmp/mnesia
NODE_NAME = publicator@127.0.0.1

APPS =   kernel stdlib crypto webtool mnesia eunit tools os_mon runtime_tools xmerl inets

REBAR_URL = https://github.com/rebar/rebar/wiki/rebar

.PHONY : all get-deps get-rebar configure compile clean test eunit ct build-erlang-plt build-plt dialyze docs start blackbox

# compile
all: compile

$(REBAR):
	rm -f rebar
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod a+x rebar

get-rebar: $(REBAR)

RELX_VERSION = 1.2.0
$(RELX):
	rm -f relx
	wget https://github.com/erlware/relx/archive/v$(RELX_VERSION).zip
	unzip v$(RELX_VERSION).zip
	cd relx-$(RELX_VERSION);make;mv ./relx ../
	rm -rf relx-$(RELX_VERSION); rm v$(RELX_VERSION).zip
	chmod a+x relx

get-relx: $(RELX)

# get dependencies
get-deps: $(REBAR)
	@$(REBAR) get-deps

configure: get-rebar get-deps

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean
	if [ -d "test" ]; then rm -f test/*.beam; fi
	rm -f erl_crash.dump

rel: $(RELX)
	rm -f publicator.tar.gz
	@$(RELX)
	cd _rel;tar -czvf publicator.tar.gz publicator;mv publicator.tar.gz ../
	rm -rf _rel
	echo "release file is publicator.tar.gz"

release: clean compile rel


test: clean compile ct

eunit: $(REBAR)
	@$(REBAR) eunit skip_deps=true

ct: $(REBAR)
	@$(REBAR) ct skip_deps=true

build-erlang-plt:
	@$(DIALYZER) --build_plt --output_plt .erlang_dialyzer.plt \
		--apps erts kernel stdlib ssl crypto

build-plt: compile
	@$(DIALYZER) --build_plt  --add_to_plt --plt .erlang_dialyzer.plt\
		--output_plt .deps_dialyzer.plt \
		--apps deps/* $(APPS)


dialyze:
	@$(DIALYZER) --src lib/*/src --plt .deps_dialyzer.plt\
		-Werror_handling \
		-Wrace_conditions -Wunmatched_returns \
		| grep -v -f ./.dialyzer-ignore-warnings
# -Wunderspecs
docs: $(REBAR)
	@$(REBAR) doc skip_deps=true

# start for development
start: compile
	erl -pa ebin deps/*/ebin \
	    -i  include deps/*/include \
	    -config sys.config \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR)"' \
	    -name $(NODE_NAME) \
	    -eval "sync:go()." \
	    -s lager \
	    -s publicator_core \
	    -s http


blackbox:
	rm -f publicator_test
	cd devutils/publicator_test/; ../../rebar compile escriptize;mv publicator_test ../../
	./publicator_test
