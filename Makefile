all: get-deps vm-args  compile-all

remake: clean-deps clean all

compile:
	- rm src/klang/kraft_scanner.erl
	- rm src/klang/kraft_parser.erl
	@(rebar skip_deps=true compile)

compile-all:
	- rm src/klang/kraft_scanner.erl
	- rm src/klang/kraft_parser.erl
	- rm ebin -r
	@(rebar compile)


win:
	- rm src\klang\kraft_scanner.erl
	- rm src\klang\kraft_parser.erl
	- rm ebin -r
	@(rebar compile) 2>NUL


get-deps:
	@(GPROC_DIST=true rebar get-deps)

update-deps:
	@(rebar get-deps)

clean:
	@(rebar clean)

clean-deps:
	@(rebar delete-deps)

xref:
	@(rebar skip_deps=true xref)

dial:
	dialyzer \
		--src -r src \
		-pa /home/niahoo/src/popos \
		-pa ebin \
		-pa ./deps/parse_trans/ebin \
		--verbose > _build/di \
	|| php priv/dialyzer-clean.php

typer:
	typer \
		src src/data src/interfaces \
		-pa /home/niahoo/src/popos \
		-pa ebin \
		-pa ./deps/parse_trans/ebin

doc:
	@(rebar skip_deps=false doc)

vm-args:

	cat priv/vm-args.src > priv/vm-args

	for i in `find -name ebin -type d`; do \
		echo '-pa' $$i >> priv/vm-args; \
	done


