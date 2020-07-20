%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(config_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("timeout.hrl").
-include("log.hrl").
-include("config.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{app_info,node_info,catalog_info}).


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------



-export([update_info/0,
	 get_info/1
	]).

-export([start/0,
	 stop/0,
	 ping/0,
	 heart_beat/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


get_info(WhichInfo)-> 
    gen_server:call(?MODULE, {get_info,WhichInfo},infinity).

ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------

update_info()->
    gen_server:cast(?MODULE, {update_info}).

heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason} 
%
%% --------------------------------------------------------------------
init([]) ->
    spawn(fun()->h_beat(?CONFIG_HEARTBEAT) end),
    {ok, #state{}}.  
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({get_info,WhichInfo},_From,State) ->
    Reply=case WhichInfo of
	      node->
		  State#state.node_info;
	      catalog->
		  State#state.catalog_info;
	      app->
		 State#state.app_info;
	      Err->
		  {error,[Err, ?MODULE,?LINE]}
	  end,

    {reply, Reply, State};

%% App spec functions


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({heart_beat,Interval}, State) ->
    
    spawn(fun()->h_beat(Interval) end),    
    {noreply, State};

handle_cast({update_info}, State) ->
     NewNodeInfo=case rpc:call(node(),config,get_info,[?CONFIG_DIR,?NODE_CONFIG_FILE]) of
		    {ok,NodeInfo}->
			NodeInfo;
		    _->
			State#state.node_info
		end,
     NewCatalogInfo=case rpc:call(node(),config,get_info,[?CONFIG_DIR,?CATALOG_CONFIG_FILE]) of
		    {ok,CatalogInfo}->
			CatalogInfo;
		    _->
			State#state.catalog_info
		end,
     NewAppInfo=case rpc:call(node(),config,get_info,[?CONFIG_DIR,?APP_CONFIG_FILE]) of
		    {ok,AppInfo}->
			AppInfo;
		    _->
			State#state.app_info
		end,
    
    NewState=State#state{node_info=NewNodeInfo,
			 app_info=NewAppInfo,
			 catalog_info=NewCatalogInfo},
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
h_beat(Interval)->

    ok=config:down_load_config(?CONFIG_URL,?CONFIG_DIR),
    true=rpc:cast(node(),config_service,update_info,[]),
    
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
