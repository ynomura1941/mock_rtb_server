all: deps debug_compile

setup: compile
	rm -Rf db/*
	erl -sname rtb_server -mnesia dir '"db"' -noinput -pa app/rtb_server/ebin/ -eval 'rtb_ad:setup([node()])' -s init stop

deps:
	./rebar get-deps

debug_compile:
	./rebar -Ddebug compile

compile: deps
	./rebar compile

doc:
	./rebar skip_deps=true doc

clean:
	./rebar skip_deps=true clean

all_clean:
	./rebar clean

distclean: clean
	rm -Rf db/*
	rm -Rf logs/*
	./rebar delete-deps

update:
	./rebar update-deps

start: all
	deps/yaws/bin/yaws -i -sname rtb_server -c etc/yaws.conf

