%% @author T520
%% @doc @todo Add description to call_server.


-module(call_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,b_party_confirmed/1,b_party_declined/1,close/1,new_call/2,relay/2]).

start_link()->
	gen_server:start_link({global,?MODULE},?MODULE, [], []).

new_call(CallerName,CalleeName)->
	gen_server:call({global,?MODULE}, {new_call,CallerName,CalleeName}).

b_party_confirmed(CallId)->
	gen_server:call({global,?MODULE}, {b_party_confirmed,CallId}).
b_party_declined(CallId)->
	gen_server:call({global,?MODULE}, {b_party_declined,CallId}).

relay(CallId,Msg)->
	gen_server:call({global,?MODULE}, {relay,CallId,Msg}).
close(CallId) -> %%{terminate,CallId}
	gen_server:call({global,?MODULE}, {terminate,CallId}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {calllist}).


init([]) ->
	process_flag(trap_exit, true),
    {ok, #state{calllist=[]}}.



handle_call({new_call,CallerName,CalleeName}, From, State=#state{calllist=CallList}) ->
	CallId = random:uniform(200000),
	io:format("Starting new call with ID ~p ~n",[CallId]),
    case call:start_link(CallId, CallerName, CalleeName) of 
		{ok,Pid} ->  {reply, ok, State#state{calllist=CallList++[{CallId,Pid}]}};
		Err -> 
			io:format("call:start_link retuened error ~p~n", [Err]),
			{reply,error,State}
	end;

handle_call({b_party_confirmed,CallId}, {FromPid,_}, State=#state{calllist=CallList}) ->
	io:format("Got b_party_confiramtion from pid ~p~n",[FromPid]),
	case proplists:get_value(CallId, CallList) of
		undefined -> {reply,call_id_does_not_exist,State};
		Pid -> call:b_party_confirmed(Pid, FromPid),
			   {reply, ok,State}
	end;

handle_call({b_party_declined,CallId}, {FromPid,_}, State=#state{calllist=CallList}) ->
	case proplists:get_value(CallId, CallList) of
		undefined -> {reply,call_id_does_not_exist,State};
		Pid -> call:b_party_declined(Pid, FromPid),
			   {reply, ok,State}
	end;

handle_call({relay,CallId,Msg}, {FromPid,_}, State=#state{calllist=CallList}) ->
	case proplists:get_value(CallId, CallList) of
		undefined -> {reply,call_id_does_not_exist,State};
		Pid -> call:relay(Pid, FromPid,Msg),
			   {reply, ok,State}
	end;
 
handle_call({terminate,CallId}, {FromPid,_}, State=#state{calllist=CallList}) ->
	case proplists:get_value(CallId, CallList) of
		undefined -> {reply,call_id_does_not_exist,State};
		Pid -> call:close(Pid, FromPid),
			   {reply, ok,State}
	end.
   

handle_cast(Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', From, _}, State=#state{calllist=CallList}) ->
	case lists:keyfind(From, 2, CallList) of 
		{CallId,From} -> {noreply, State#state{calllist=proplists:delete(CallId, CallList)}};
		false -> io:format("Process ~p died but it was not found in call list~n", [From]),
				 {noreply, State}
	end.
    


terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


