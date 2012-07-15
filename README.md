
# About 

The purpose of this project is just to compare few 
scripting engines for Erlang. 

The goal is to answer next questions:

1. How much memory is needed for each engine?
2. How many concurrent instances of engine can be run?
3. How easy to use is each engine? 

## erlv8

Repo: https://github.com/beamjs/erlv8
About: V8 Integration with Erlang. Implemented in same way as erlang_js - linked-in driver

Answers: 
1. How much memory:
 	10 loaded scripts consumed 6700M of virtual memory
 	20 loaded scripts consumed 12GB of virtual memory
 	
2. How many concurrent instances:
	100 with default settings	 
3. How easy:
	Very easy for simple cases 


## erlang_js 

Repo: https://github.com/basho/erlang_js
About: "erlang_js is a linked-in driver, API, and OTP application that embeds Mozilla’s Spidermonkey Javascript Virtual Machine in Erlang."

Note: to build http://lists.basho.com/pipermail/riak-users_lists.basho.com/2010-February/000436.html may be useful

Answers: 

1. How much memory:
	Seems does not do pre-allocation. 400 instaces consumed about 150M of virtual memory

2. How many concurrent instances:
	As much as erlang VM allows

3. How easy:
	There are lack of documentation, but it is quite easy to find examples. 

## luerl

Repo: https://github.com/rvirding/luerl/
About: Lua interpreter written in Erlang

Answers: 

1. How much memory:
 	1000 loaded scripts consumed about 2Mb of virtual memory.
 	
2. How many concurrent instances:
	As much as erlang VM allows. 
	
3. How easy:
	Suitable only for cases when result is needed (like some calculations). 
	To make lua script interact with outer world - changes are needed in luerl
	

## erl-lua 

Repo: https://github.com/raycmorgan/erl-lua
About: yet another Lua integration. At this time erl-lua is more like erlv8 - part of source is in c 

Because erl-lua seems not maintained, I've used fork https://github.com/Motiejus/erl-lua wich seems more alive. 

<b>Note: to build erlualib you need lua been install + libluabind-dev (c++ bindings)</b>

One more interesting thing: it is planned to use Lua for implementing gen_* behaviours (http://m.jakstys.lt/tech/2012/06/erlang-behaviours-in-lua/)

Answers: 

1. How much memory: 1000 loaded scripts consumed about 5Mb of virtual memory.
2. How many concurrent instances: As much as erlang VM allows. 
3. How easy: Not usable, currently. 

erl-lua is just driver for lualib. 


