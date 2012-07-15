

erl -sname luerl_try -pa ../erl_lua_try/ebin -pa ./deps/*/ebin -pa ../lua_scripts/ \
	 -s erl_lua_try_app

