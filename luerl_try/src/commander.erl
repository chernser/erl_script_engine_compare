%%%-------------------------------------------------------------------
%%% @author  <chernser@Zeus>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by  <chernser@Zeus>
%%%-------------------------------------------------------------------
-module(commander).

-behaviour(gen_server).

%% API
-export([start_link/0, list/0, load/1, unload/1, execute/1]).

%% for spawn_link
-export([lua_vm_loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
		  available_scripts,
		  instances	= []	  
		 }).


-record(script, { name :: binary(), 
				  filename :: binary() }).

-record(instance, { name :: binary(), %% script name
					vm_pid :: pid() %% vm_pid of process 
				  }).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%  Lists all available scripts
%% @end
%%--------------------------------------------------------------------
list() ->
	gen_server:call(?SERVER, list).

%%--------------------------------------------------------------------
%% @doc
%%  Loads script into new instance of JS VM and returns 
%%   Reference to it 
%% @end
%%--------------------------------------------------------------------
load(Script) ->
	gen_server:call(?SERVER, {load, Script}).

%%--------------------------------------------------------------------
%% @doc
%%  Loads script into new instance of JS VM
%% @end
%%--------------------------------------------------------------------
unload(Ref) ->
	gen_server:call(?SERVER, {unload, Ref}).


%%--------------------------------------------------------------------
%% @doc
%%  Executes script
%% @end
%%--------------------------------------------------------------------
execute(Ref) ->
	gen_server:call(?SERVER, {execute, Ref}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	%% Load scripts 
	io:fwrite("Starting commander 1~n"), 
	ScriptsDir =filename:absname("../lua_scripts/"),
	io:fwrite("Loading scripts from: ~p~n", [ScriptsDir]),
	case file:list_dir(ScriptsDir) of 
		{error, Reason} ->
			io:fwrite("Failed to load scripts from dir: ~p for reason ~p~n",
					  [ScriptsDir, Reason]),
			{stop, Reason};
		{ok, Files} ->
			
			Scripts = orddict:from_list(
						[ { filename:basename(F, ".lua"), 
							#script{name = filename:basename(F, ".lua"),
									filename = ScriptsDir ++ "/" ++ F}
						  }
						  || F <- Files, filename:extension(F) =:= ".lua"]),
			
			io:fwrite("There are next available scripts: ~p~n", [Scripts]),
			{ok, #state{ available_scripts = gb_trees:from_orddict(Scripts) }}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load, ScriptName}, _From, 
			State0 = #state{available_scripts = Scripts}) ->
	case gb_trees:lookup(ScriptName, Scripts) of 
		none ->
			{reply, script_not_found, State0};
		{value, Script} ->			
			{ok, Ref, State1} = load_script(Script, State0),
			{reply, {ok, Ref}, State1}
	end;
handle_call({unload, Ref}, _From, State) ->
	Reply = ok,
	{reply, Reply, State};
handle_call({execute, Ref}, _From, State0) ->
	{Reply, State1} = execute(Ref, State0),
	{reply, Reply, State1};
handle_call(list, _From, State) ->
	Reply = {ok, gb_trees:keys(State#state.available_scripts)},
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%%   Create new VM instance 
%% @end
%%--------------------------------------------------------------------
load_script(#script{filename = File, name = Name}, State0) ->
	{ok, Content} = file:read_file(File),
	Source = binary_to_list(Content),		
	{ok, Chunk} = luerl:load(Source),
	State = luerl:init(),
	VM = spawn_link(?MODULE, lua_vm_loop, [State]),
	Instance = #instance{name = Name, vm_pid = VM},
	{ok, VM, State0#state{instances = State0#state.instances ++ [Instance]}}.
	

%%--------------------------------------------------------------------
%% @doc
%%   Executes predefined sequence of script 
%% @end
%%--------------------------------------------------------------------
execute(VM, State0) ->
	VM ! {execute, self()},
	receive 
		{result, Result} ->
			io:fwrite("Execution result ~p~n", [Result])
	after 1000 ->
			ok
	end,		
	{ok, State0}.

				   
lua_vm_loop(State0) ->
	receive 
		{execute, Pid} ->
			Result = luerl:eval("a = 3 + 4\n print(a)", State0),
			Pid ! {result, Result},
			lua_vm_loop(State0);
		terminate ->
			ok;
		_ ->
			lua_vm_loop(State0)
	end.
