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
-export([down_load_config/3,get_info/2,update_info/3]).

%% ====================================================================
%% External functions
%% ====================================================================



down_load_config(dir,Path,Dir)->
    os:cmd("rm -rf "++Dir),
    os:cmd("mkdir "++Dir),
    Source=filename:join([Path,Dir,"*"]),
    os:cmd("cp "++Source++" "++Dir),
    timer:sleep(100),    
    ok;

down_load_config(git,GitUrl,Dir)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitUrl),
    timer:sleep(100),    
    ok.
get_info(Dir,FileName)->
    {R,Info}=file:consult(filename:join(Dir,FileName)),
    {R,Info}.    

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
