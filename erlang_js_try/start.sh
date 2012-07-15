

erl -sname erlv8_try -pa ../erlang_js_try/ebin -pa ./deps/*/ebin -pa ../js_scripts/ \
	 -s erlang_js -s erlang_js_try_app

