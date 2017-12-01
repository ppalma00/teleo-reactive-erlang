%% @author Usuario_PC
%% @doc @todo Add description to example.


-module(bs).

-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([start_link/0, 
		 add_belief/1, 
		 update_belief/1, 
		 remove_belief/1, 
		 get_belief/1, 
		 is_belief/1,
		 get_bs/0,
		 stop/0]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% ====================================================================
%% API Function Definitions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_belief(Belief)->
	gen_server:cast(?MODULE,{add,Belief}).

update_belief(Belief)->
	gen_server:cast(?MODULE, {update, Belief}).

remove_belief(Belief)->
	gen_server:cast(?MODULE, {remove, Belief}).

get_belief(Belief)->
	gen_server:call(?MODULE,{get,Belief}).

is_belief(Key)->
	gen_server:call(?MODULE,{is_belief,Key}).

get_bs()->
	gen_server:call(?MODULE,{get_bs}).

stop()->
	gen_server:cast(?MODULE,stop).


%% ====================================================================
%% gen_server Function Definitions
%% ====================================================================


init([])->
	{ok, #{see=>
			   [],
		   collected=>
			   0,
		   holding=>[],
		   over_drop=>[],
		   last_executed=>
			   {[],[],[]},
		   executing=>
			   {[],[],[]},
		   to_execute=>
			   {[],[],[]},
		   timer=>
			   [],
		   priority=>
			   [{collect_bottles,[collected, holding_over_drop, holding, true,[]]},
				{drop_and_leave,[gripper_open,true,[]]},
				{leave_drop,[not_see,see_drop,see_robot,[]]},
				{get_to_drop,[over_drop, see_drop, true,[]]},
				{get_bottle,[holding, touching_gripper_open, touching, see_bottle, true,[]]},
				{[],[]}]}}.

handle_call({get_bs},_From,State)->
	{reply,State,State};

handle_call({get,Belief},_From,State)->
	{reply,maps:get(Belief,State),State};

handle_call({is_belief,Key},_From,State)->
	{reply,maps:is_key(Key,State),State}.

handle_cast(stop,State)->
	{stop,normal,State};

handle_cast({add,{Key,Value}},State)->
	io:format("Belief added: ~p.~n",[{Key,Value}]),
	{noreply, maps:put(Key,Value,State)};

handle_cast({update,{Key,Value}},State)->
	io:format("Belief updated: ~p.~n",[{Key,Value}]),
	{noreply,State#{Key => Value}};

handle_cast({remove,Key},State)->
	io:format("Belief removed: ~p.~n",[Key]),
	{noreply,maps:remove(Key, State)}.

handle_info(Info, State) ->     
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          

terminate(_Reason, _State) -> 
    error_logger:info_msg("terminating~n"), 
    ok.                       

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.      