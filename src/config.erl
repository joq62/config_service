%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(config).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("config.hrl").
-include("log.hrl").
%-compile(export_all).
-export([update_info/3]).

%% ====================================================================
%% External functions
%% ====================================================================

update_info(GitUrl,Dir,FileName)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitUrl),
    timer:sleep(100),
    {R,Info}=file:consult(filename:join(Dir,FileName)),
    {R,Info}.

%% --------------------------------------------------------------------
%% 
%% 
%% {"master_sthlm_1",'master_sthlm_1@asus'}
%% --------------------------------------------------------------------
%% doc: get(ServiceId) returns of a list of nodes that have ServiceId all running applications
