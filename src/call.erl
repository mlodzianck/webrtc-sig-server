%% @author T520
%% @doc @todo Add description to call.


-module(call).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,bparty_requested/2,confirmed/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3,b_party_confirmed/2,b_party_declined/2,close/2,relay/3]).

start_link(CallId,AName,BName) ->
	gen_fsm:start_link(?MODULE, [CallId,AName,BName], []).

b_party_confirmed(Pid,FromPid) ->
	gen_fsm:send_event(Pid, {bparty_confirmed,FromPid}).

b_party_declined(Pid,FromPid) ->
	gen_fsm:send_event(Pid, {bparty_declined,FromPid}).

relay(Pid,FromPid,Msg) ->
	gen_fsm:send_event(Pid, {FromPid,relay,Msg}).

close(Pid,FromPid) ->
	gen_fsm:send_event(Pid, {terminate,FromPid}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {apartyname,apartypid,bpartyname,bpartypid,callid}).
init([CallId,AName,BName]) when is_binary(AName),is_binary(BName)->
	init([CallId,binary_to_list(AName),binary_to_list(BName)]);

init([CallId,AName,BName]) when is_list(AName),is_list(BName)->
	APartyPidR = subs_db:get_ws_pid(AName),
	BPartyPidR = subs_db:get_ws_pid(BName),
	
	case {APartyPidR,BPartyPidR} of
		{{ok,APid},{ok,BPid}} ->
						ws_sig:send_call_request(BPid, CallId, AName),
						ws_sig:send_call_request_sent(APid, CallId, BName),
    					{ok, bparty_requested, #state{apartyname=AName,apartypid=APid,bpartyname=BName,bpartypid=BPid,callid=CallId},20000};
		_->{stop, error_getting_pid_of_parties}
	end.



bparty_requested({bparty_confirmed,BPid}, StateData=#state{apartypid=APid,bpartypid=BPid,callid=CallId,bpartyname=BName,apartyname=AName}) ->
	ws_sig:send_b_party_confirmation(APid, CallId),
	subs_db:update_state(AName, busy),
	subs_db:update_state(BName, busy),
	subs_db:add_call_to_sub(AName, CallId),
	subs_db:add_call_to_sub(BName, CallId),
    {next_state, confirmed, StateData};


bparty_requested({bparty_declined,BPid}, StateData=#state{apartypid=APid,bpartypid=BPid,callid=CallId}) ->
	ws_sig:send_b_party_declined(APid, CallId),
    {stop, bparty_declined, StateData};

bparty_requested(timeout,StateData=#state{callid=CallId,apartypid=APid,bpartypid=BPid}) ->
	ws_sig:send_b_party_confirmation_timeout(APid, CallId),
	ws_sig:send_confirmation_cancelation(BPid, CallId),
	
	io:format("B-Party have not confirmed call, exiting call no . ~p~n",[CallId]),
	{stop, b_confirmation_timeout, StateData}.



confirmed({FromPid,relay,Msg},StateData=#state{apartypid=FromPid,bpartypid=BPid,callid=CallId}) ->
	ws_sig:send_relayed_message(BPid,CallId,Msg),
	{next_state, confirmed, StateData};

confirmed({FromPid,relay,Msg},StateData=#state{apartypid=APid,bpartypid=FromPid,callid=CallId}) ->
	ws_sig:send_relayed_message(APid,CallId,Msg),
	{next_state, confirmed, StateData};

confirmed({FromPid,relay,_Msg},StateData) ->
	io:format("Hey! PID ~p isn't A-party pid nor B-Party pid!~n", [FromPid]),
	{next_state, confirmed, StateData};

confirmed({terminate,_FromPid},StateData) ->
	
	{stop, normal, StateData}.



handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


terminate(Reason, StateName, StateData=#state{apartypid=APid,bpartypid=BPid,callid=CallId,apartyname=AName,bpartyname=BName}) ->
	subs_db:update_state(AName, idle),
	subs_db:update_state(BName, idle),
	ws_sig:send_termination(APid, CallId),
	ws_sig:send_termination(BPid, CallId),
	subs_db:remove_call_from_sub(AName, CallId),
	subs_db:remove_call_from_sub(BName, CallId),
    ok.



code_change(OldVsn, StateName, StateData, Extra) ->
	io:format("Code change ~n"),
	
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================


