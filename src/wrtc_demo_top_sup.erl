%% @author T520
%% @doc @todo Add description to wrtc_demo_top_sup.


-module(wrtc_demo_top_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
start_link()->
	supervisor:start_link(?MODULE, []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    YBed = {ybed, {ybed_sup,start_link,[]},
            permanent,2000,supervisor,[ybed,ybed_sup]},
	 Call_server_spec = {call_server,{call_server,start_link,[]},
	      permanent,2000,worker,[call_server]},
	 Subs_db_spec = {subs_db,{subs_db,start_link,[]},
	      permanent,2000,worker,[subs_db]},
	
    {ok,{{one_for_all,0,1}, [YBed,Call_server_spec,Subs_db_spec]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


