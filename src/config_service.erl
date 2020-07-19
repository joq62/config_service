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
-record(state,{service_list}).


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------



-export([update/1
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


ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------

update(ServiceList)->
    gen_server:cast(?MODULE, {update,ServiceList}).

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
    {ok, #state{service_list=[]}}.  
    
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

handle_cast({update,ServiceList}, State) ->
    NewState=State#state{service_list=ServiceList},
    {noreply, NewState};

handle_cast({add,_ServiceId,_Node}, State) ->

    {noreply, State};

handle_cast({delete,_ServiceId,_Node}, State) ->
 
    {noreply, State};


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

    
    % Update catalog.config and send to service X
    config:update_info(?CATALOG_CONFIG_URL,?CATALOG_CONFIG_DIR,?CATALOG_CONFIG_FILE),
    {ok,CatalogBinary}=file:read_file(?CATALOG_CONFIG_FILE),
    InstancesOrchistrate=vm_service:get_instance("orchistrate_service"),
    [rpc:call(Node,orchistrate_service,update_catalog,[?CATALOG_CONFIG_FILE,CatalogBinary],5000)||{_,Node}<-InstancesOrchistrate],
    InstancesVm=vm_service:get_instance("vm_service"),
    [rpc:call(Node,vm_service,update_catalog,[?CATALOG_CONFIG_FILE,CatalogBinary],5000)||{_,Node}<-InstancesVm],

    % Update app.config and send to service X
    config:update_info(?APP_CONFIG_URL,?APP_CONFIG_DIR,?APP_CONFIG_FILE),
    % Update node.config and send to service X

    config:update_info(?NODE_CONFIG_URL,?NODE_CONFIG_DIR,?NODE_CONFIG_FILE),
    ServiceList=dns:update(),
    dns_service:update(ServiceList),
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
