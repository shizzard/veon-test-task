REBAR = ./rebar3

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

spaces:
	build/tabs_to_spaces.sh