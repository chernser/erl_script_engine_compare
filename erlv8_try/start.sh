

erl -sname erlv8_try -pa ../erlv8_try/ebin -pa ./deps/*/ebin -pa ../js_scripts/ \
	 -s erlv8 -s erlv8_try_app 

