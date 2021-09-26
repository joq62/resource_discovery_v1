%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(rd_test).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------



%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start pod_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pod_0(),
  %  io:format("~p~n",[{"Stop pod_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start host_nodes_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=host_nodes_0(),
  %  io:format("~p~n",[{"Stop host_nodes_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

  %  io:format("~p~n",[{"Start cluster_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=cluster_0(),
 %   io:format("~p~n",[{"Stop cluster_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start load_app_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=load_app_0(),
%    io:format("~p~n",[{"Stop load_app_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

 
   io:format("~p~n",[{"Start pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pass_0(),
   io:format("~p~n",[{"Stop pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

   
      %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_0()->
    % Application interaction with rd
    % Add target resources
    ok=rd:add_target_resource_type(kvs),
    ok=rd:add_target_resource_type(balcony_web),
    ok=rd:add_target_resource_type(balcony_hw),
    
    %% Add local resources
    rd:add_local_resource(balcony_controller,node1),
    rd:add_local_resource(balcony_web,node2),
    rd:add_local_resource(kvs,node()),
    
    %% Trade
    ok=rd:trade_resources(),
    timer:sleep(1000),

    %% check 2
    ['test@joq62-X550CA']=rd:fetch_resources(kvs),
    [node2]=rd:fetch_resources(balcony_web),
    {error,_}=rd:fetch_resources(balcony_hw),
    

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pod_0()->
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_1()->
   
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_3()->

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
  
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_5()->
  
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->

  
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
