lambdatools_src := $(wildcard lib/*ex) \
	$(wildcard lib/**/*.ex) \
	mix.exs

.PHONY: all test testex iex clean wipe

all: ebin 

ebin: $(lambdatools_src)
	@mix do deps.get, compile

testex: all
	@ERL_LIBS=.:deps MIX_ENV=test iex --sname lambdatools_console --erl "-config sys -s Elixir-ExLambda -boot start_sasl"

iex: all
	@ERL_LIBS=.:deps MIX_ENV=dev iex --sname lambdatools_console --erl "-config sys -s Elixir-ExLambda -boot start_sasl"

clean:
	@mix clean

wipe: clean
	@rm -rf ./deps/*
