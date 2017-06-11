REBAR = ./rebar3

.PHONY: all get-deps compile release shell clean dialyze run-release run-tests

all: get-deps compile release

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

release: compile
	$(REBAR) release

shell: compile
	$(REBAR) shell

clean:
	$(REBAR) clean

dialyze:
	$(REBAR) dialyzer

run-release: release
	_build/default/rel/veon/bin/veon console

run-tests:
	opt/01_movie_create_test.py
	opt/02_movie_retrieve_test.py
	opt/03_movie_reserve_test.py
	opt/04_movie_reserve_remove_test.py
