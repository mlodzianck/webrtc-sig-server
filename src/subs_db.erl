%% @author T520
%% @doc @todo Add description to subs_db.


-module(subs_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_sub/2,get_all_subs/0,add_state_subscription_fun/2,delete/1,delete_state_subscription_fun/1,
		get_state/1,get_ws_pid/1,update_state/2,start_link/0,add_call_to_sub/2,remove_call_from_sub/2]).


add_sub(NickName,Pid) ->
	gen_server:call({global,?MODULE}, {add,Pid,NickName}).
get_ws_pid(NickName)->
	gen_server:call({global,?MODULE}, {get_ws_pid,NickName}).
get_state(NickName)->
	gen_server:call({global,?MODULE}, {get_state,NickName}).
update_state(NickName,SubState)->
	gen_server:call({global,?MODULE}, {update_state,NickName,SubState}).
delete(NickName) ->
	gen_server:call({global,?MODULE}, {delete,NickName}).
get_all_subs()->
	gen_server:call({global,?MODULE}, {get_all_subs}).

add_state_subscription_fun(Tag,Fun)->
	gen_server:call({global,?MODULE}, {add_state_subscription_fun,Tag,Fun}).
delete_state_subscription_fun(Tag)->
	gen_server:call({global,?MODULE}, {delete_state_subscription_fun,Tag}).

add_call_to_sub(NickName,CallId) ->
	gen_server:call({global,?MODULE}, {add_call_to_sub,NickName,CallId}).
remove_call_from_sub(NickName,CallId)->
	gen_server:call({global,?MODULE},{remove_call_from_sub,NickName,CallId}).
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE, [], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {subslist=[],state_subscribers=[],active_calls=[]}).

-record(subscriber, {nickname,state,ws_pid}).
init([]) ->
	
    {ok, #state{}}.

handle_call({add_call_to_sub,NickName,CallId},_,State=#state{active_calls=ActiveCallsList}) ->
	NewActiveCalls = case proplists:get_value(NickName, ActiveCallsList) of
						 undefined -> ActiveCallsList++[{NickName,[CallId]}];
						 List -> proplists:delete(NickName, ActiveCallsList)++[{NickName,List++[CallId]}]
					 end,
	{reply, ok, State#state{active_calls=NewActiveCalls}};

handle_call({remove_call_from_sub,NickName,CallId},_,State=#state{active_calls=ActiveCallsList}) ->
	NewActiveCalls = case proplists:get_value(NickName, ActiveCallsList) of
						 undefined -> ActiveCallsList;
						 List -> proplists:delete(NickName, ActiveCallsList)++[{NickName,lists:delete(CallId, List)}]
					 end,
	{reply, ok, State#state{active_calls=NewActiveCalls}};
	


handle_call({add,WSPid,NickName}, _, State=#state{subslist=Proplist,state_subscribers=StateSubs}) ->
	{Reply,Newlist} = case proplists:get_value(NickName,Proplist) of 
		undefined -> 
			notify_subs(NickName, idle, StateSubs),
			{ok,Proplist++[  {NickName,#subscriber{nickname=NickName,state=idle,ws_pid=WSPid}}  ]};
		 _ -> {nickname_exists,Proplist}
	end,
    {reply, Reply, State#state{subslist=Newlist}};

handle_call({get_ws_pid,NickName}, _, State=#state{subslist=Proplist}) ->
	Reply = case proplists:get_value(NickName,Proplist) of 
		#subscriber{ws_pid=Pid} -> {ok,Pid};
		_-> nickname_doesn_not_exist
	end,
    {reply, Reply, State};

handle_call({get_state,NickName}, _, State=#state{subslist=Proplist}) ->
	Reply = case proplists:get_value(NickName,Proplist) of 
		#subscriber{state=Substate} -> {ok,Substate};
		_-> nickname_doesn_not_exist
	end,
    {reply, Reply, State};

handle_call({update_state,NickName,SubState}, _, State=#state{subslist=Proplist,state_subscribers=StateSubs}) ->
	{Reply,NewList}= case proplists:get_value(NickName,Proplist) of 
		Sub=#subscriber{} -> 
			notify_subs(NickName, SubState, StateSubs),
			{ok,proplists:delete(NickName, Proplist)++[{NickName,Sub#subscriber{state=SubState}}]};
		_-> {nickname_doesn_not_exist,Proplist}
	end,
    {reply, Reply, State#state{subslist=NewList}};

handle_call({delete,NickName}, _, State=#state{subslist=Proplist,state_subscribers=StateSubs,active_calls=ActiveCallsList}) ->
	{Reply,NewList}= case proplists:get_value(NickName,Proplist) of 
		#subscriber{} -> 
			SubscriberCallList = proplists:get_value(NickName, ActiveCallsList, []),
			[call_server:close(CallId) || CallId <-SubscriberCallList],
			notify_subs(NickName, remove, StateSubs),
			{ok,proplists:delete(NickName, Proplist)};
		_-> nickname_doesn_not_exist
	end,
    {reply, Reply, State#state{subslist=NewList}};

handle_call({get_all_subs}, _, State=#state{subslist=Proplist}) ->
	Fun = fun({NickName,#subscriber{state=SubState}}) -> {NickName,SubState} end,
	Reply= {ok,lists:map(Fun, Proplist)},
    {reply, Reply, State};

handle_call({add_state_subscription_fun,Tag,Fun}, _, State=#state{state_subscribers=StateSubscribers}) ->
	{Reply,NewStateSubscribers} = case proplists:get_value(Tag, StateSubscribers) of 
									  undefined -> {ok,StateSubscribers++[{Tag,Fun}]};
									  _->{tag_already_registered,StateSubscribers}
								  end,
    {reply, Reply, State#state{state_subscribers=NewStateSubscribers}};

handle_call({delete_state_subscription_fun,Tag}, _, State=#state{state_subscribers=StateSubscribers}) ->
    {reply, ok, State#state{state_subscribers=proplists:delete(Tag, StateSubscribers)}};
	



handle_call(Request, From, State) ->
    io:format("Unhandled call ~p~n",[Request]),
    {reply, error, State}.



handle_cast(Msg, State) ->
	 io:format("Unhandled cast ~p~n",[Msg]),
    {noreply, State}.

handle_info(Info, State) ->
	io:format("Unhandled info ~p~n",[Info]),
    {noreply, State}.


terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
notify_subs(NickName,SubState,StateSubs)->
	Notification = fun() -> [catch F(NickName,SubState)|| {_Tag,F} <-StateSubs] end,
	spawn(Notification).

