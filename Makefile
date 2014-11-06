REBAR=`which rebar || printf ./rebar`
MODULE=pri_server

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) -v compile

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit

clean:
	@$(REBAR) clean

APPS = kernel stdlib 
PLT = $(HOME)/.$(REPO)_dialyzer.plt

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) \
		ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) \
		ebin

dialyze: compile
	dialyzer -Wno_return --plt $(PLT) ebin
