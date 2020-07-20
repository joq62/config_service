%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(config_test).  
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-export([start/0]).
%-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================

%% 
%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("basic_test"),
    ?assertEqual(ok,basic_test()),

    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

basic_test()->
    timer:sleep(3*1000),
   ?assertEqual([{"vm_service",sthlm_1@asus},
		 {"log_service",sthlm_1@asus}],
		config_service:get_info(app)),
    ?assertEqual([{"vm_service",git,"https://github.com/joq62/"},
		  {"log_service",git,"https://github.com/joq62/"},
		  {"orchistrate_service",git,"https://github.com/joq62/"},
		  {"oam_service",git,"https://github.com/joq62/"},
		  {"adder_service",git,"https://github.com/joqerlang/"},
		  {"multi_service",git,"https://github.com/joqerlang/"},
		  {"subtract_service",git,"https://github.com/joqerlang/"},
		  {"divi_service",git,"https://github.com/joqerlang/"}],
		 config_service:get_info(catalog)),    

   ?assertEqual([{"sthlm_1",sthlm_1@asus},{"test_agent",test_agent@asus}],
		config_service:get_info(node)),
    ok.


