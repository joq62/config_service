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
%-include("timeout.hrl").
-include("log.hrl").
%-include("config.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{app_info,catalog_info,node_info,
	       config_type,config_path,config_dir,
	       catalog_file,app_file,node_file}).


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
    {ok,HbInterval}=application:get_env(hb_interval),
    {ok,ConfigType}=application:get_env(config_type),
    {ok,ConfigPath}=application:get_env(config_path),
    {ok,ConfigDir}=application:get_env(config_dir),

    {ok,CatalogFile}=application:get_env(catalog_file),
    {ok,AppFile}=application:get_env(app_file),
    {ok,NodeFile}=application:get_env(node_file),
    
    spawn(fun()->h_beat(HbInterval,ConfigType,ConfigPath,ConfigDir) end),
    {ok, #state{config_type=ConfigType,
		config_path=ConfigPath,
	        config_dir=ConfigDir,
	        catalog_file=CatalogFile,
		app_file=AppFile,
		node_file=NodeFile
	       }}.  
    
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
	      node_info->
		  State#state.node_info;
	      catalog_info->
		  State#state.catalog_info;
	      app_info->
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
    
    spawn(fun()->h_beat(Interval,State#state.config_type,
			State#state.config_path,State#state.config_dir) end),    
    {noreply, State};

handle_cast({update_info}, State) ->
     NewNodeInfo=case rpc:call(node(),config,get_info,[State#state.config_dir,State#state.node_file]) of
		    {ok,NodeInfo}->
			NodeInfo;
		    _->
			State#state.node_info
		end,
     NewCatalogInfo=case rpc:call(node(),config,get_info,[State#state.config_dir,State#state.catalog_file]) of
		    {ok,CatalogInfo}->
			CatalogInfo;
		    _->
			State#state.catalog_info
		end,
     NewAppInfo=case rpc:call(node(),config,get_info,[State#state.config_dir,State#state.app_file]) of
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
h_beat(Interval,Type,ConfigPath,ConfigDir)->

    ok=config:down_load_config(Type,ConfigPath,ConfigDir),
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
