all: compile

get-deps:
	@./rebar get-deps

del-deps:
	@./rebar del-deps

compile: get-deps
	@./rebar compile

clean:
	@./rebar clean
	@rm -f itest/*.beam

compile_itest:
	@cd itest;erlc -I ../include *.erl

itest: compile compile_itest
	@erl -noshell -pa ebin -pa deps/protobuffs/ebin -pa itest -eval 'gobo_itest:start().' -s erlang halt
