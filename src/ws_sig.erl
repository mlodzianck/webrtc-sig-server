-module(ws_sig).
-include("yaws_api.hrl").

-export([out/1,handle_message/2]).
-export([send_call_request/3,send_call_request_sent/3,send_b_party_confirmation/2,
		 send_b_party_declined/2,send_b_party_confirmation_timeout/2,send_termination/2,send_confirmation_cancelation/2,send_relayed_message/3]).
-record(state, {subscriber_name=undefined,arg=undefined}).

out(A)->
		io:format("Opening connection for ~p~n",[A#arg.client_ip_port]),
		io:format("Arg ~p~n",[A]),
        {websocket, ?MODULE, [{callback, {advanced, #state{arg=A}}}]}.
handle_message(#ws_frame_info{opcode=close}, #state{subscriber_name=undefined,arg=A}) ->
	SelfPid=self(),
	io:format("Closing connection for ~p~n",[A#arg.client_ip_port]),
	subs_db:delete_state_subscription_fun(SelfPid),
	{close, normal};
handle_message(#ws_frame_info{opcode=close}, #state{subscriber_name=SName}) ->
	SelfPid=self(),
	subs_db:delete(SName),
	subs_db:delete_state_subscription_fun(SelfPid),
	{close, normal};


handle_message(Frame=#ws_frame_info{opcode=text, data=Data}, State) ->
      
        case json:decode_string(binary_to_list(Data)) of 
                {ok,{struct,Proplist}} -> handle_text_message(Proplist,State);
                _->  io:format("Got unparsable JSON message ~p",[(Data)]),
                         {noreply, State}
        end;

handle_message(Frame,State) ->
   io:format("Unknown frame ~p state = ~p",[Frame#ws_frame_info.opcode,State]),
   {noreply, State}.

handle_text_message([{command,"register"}|Params],State=#state{subscriber_name=undefined}) ->
	Nick = proplists:get_value(name, Params),
	SelfPid =self(),
	case subs_db:add_sub(Nick, self()) of
		ok ->
			NotFun = fun(Nick,SubState)-> 
							send_state_update(SelfPid,Nick,SubState)
					  end,
			subs_db:add_state_subscription_fun(SelfPid, NotFun),
			{ok,People_} = subs_db:get_all_subs(),
			%%adjust format of presence list
			People = [{list_to_binary(Name),atom_to_binary(Atom, utf8)} || {Name,Atom} <- People_],
			Msg= json:encode({struct,[{<<"command">>, <<"register_response">>},
									  {<<"status">>, <<"success">>},
									   {<<"name">>, list_to_binary(Nick)},
									  {<<"people">>,{struct,People}}]}),
			
		
			{reply,{text, list_to_binary(Msg)}, State#state{subscriber_name=Nick}};
		nickname_exists ->
			Msg= json:encode({struct,[{<<"command">>, <<"register_response">>},
									  {<<"status">>, <<"failed">>},
										{<<"reason">>, <<"nickname already registered">>}]}),
			{reply, {text, list_to_binary(Msg)},State};
		_ ->
			Msg= json:encode({struct,[{<<"command">>, <<"register_response">>},
									  {<<"status">>, <<"failed">>},
										{<<"reason">>, <<"unspecified error">>}]}),
			{reply, {text, list_to_binary(Msg)},State}
	end;
handle_text_message([{command,"register"}|_Params],State) ->
	Msg= json:encode({struct,[{<<"command">>, <<"register_response">>},
									  {<<"status">>, <<"failed">>},
										{<<"reason">>, <<"can not register twice">>}]}),
			{reply, {text, list_to_binary(Msg)},State};


handle_text_message(_,State=#state{subscriber_name=undefined}) ->
	Msg= json:encode({struct,[{<<"command">>, <<"registration_required">>}]}),
	{reply,{text, list_to_binary(Msg)}, State};

handle_text_message([{command,"call"}|Params],State=#state{subscriber_name=AName}) ->
	CalleeName = proplists:get_value(name, Params),
	case call_server:new_call(AName, CalleeName) of 
		ok ->{noreply, State};
		error -> Msg= json:encode({struct,[{<<"command">>, <<"call_failed">>},
										   {<<"callee">>,list_to_binary(CalleeName)}]}),
					{reply,{text, list_to_binary(Msg)}, State}
	end;
handle_text_message([{command,"confirm_call"}|Params],State) ->
	CallId = proplists:get_value(call_id, Params),
	SelfPid = self(),
	io:format("Confirming call from PID ~p~n",[SelfPid]),
	call_server:b_party_confirmed(CallId),
	{noreply, State};
handle_text_message([{command,"end_call"}|Params],State) ->
	CallId = proplists:get_value(call_id, Params),
	SelfPid = self(),
	io:format("Tering down call from PID ~p~n",[SelfPid]),
	call_server:close(CallId),
	{noreply, State};
handle_text_message([{command,"relay"}|Params],State) ->
	Payload = proplists:get_value(payload, Params),
	CallId = proplists:get_value(call_id, Params),
	
	call_server:relay(CallId,Payload),
	{noreply, State}.


send_call_request(WSPid,CallId,APartyName)->
	Msg= json:encode({struct,[{<<"command">>, <<"call_request">>},
												  {<<"call_id">>, CallId},
												  {<<"caller">>,  list_to_binary(APartyName)}]}),
	yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_call_request_sent(WSPid,CallId,BPartyName)->
	Msg= json:encode({struct,[{<<"command">>, <<"call_request_sent">>},
												  {<<"call_id">>, CallId},
												  {<<"callee">>,  list_to_binary(BPartyName)}]}),
	yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_b_party_confirmation(WSPid,CallId)->
	Msg= json:encode({struct,[{<<"command">>, <<"b_party_confirmation">>},
												  {<<"call_id">>, CallId}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_b_party_declined(WSPid,CallId)->
	Msg= json:encode({struct,[{<<"command">>, <<"b_party_declined">>},
												  {<<"call_id">>, CallId}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_b_party_confirmation_timeout(WSPid, CallId) ->
	Msg= json:encode({struct,[{<<"command">>, <<"b_party_confirmation_timeout">>},
												  {<<"call_id">>, CallId}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_termination(WSPid,CallId)->
	Msg= json:encode({struct,[{<<"command">>, <<"termniate">>},
												  {<<"call_id">>, CallId}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_state_update(WSPid,Nick,SubState) ->
	Msg= json:encode({struct,[{<<"command">>, <<"subscriber_state_update">>},
								{<<"nickname">>, Nick},
							 { <<"status">>,atom_to_binary(SubState, utf8)}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_confirmation_cancelation(WSPid, CallId)->
		Msg= json:encode({struct,[{<<"command">>, <<"call_request_cancel">>},
								{<<"call_id">>, CallId}]}),
						yaws_websockets:send(WSPid, {text,list_to_binary(Msg)}).

send_relayed_message(WSPid,CallId,Payload) ->
	
	Msg= json:encode({struct,[{<<"command">>, <<"relayed">>},
								{<<"call_id">>, CallId},
							  {<<"payload">>,Payload}]}),
	
						yaws_websockets:send(WSPid,{text,list_to_binary(Msg)} ).
	