%% @author Elias Antolinos
%% @doc @todo Module which contains and manages the BeliefStore.


-module(bs).

-behaviour(gen_server).


%%----------------------
%% API Function Exports
%%----------------------

-export([start_link/0, 
		 add_belief/1, 
		 update_belief/1, 
		 remove_belief/1,
		 remove_one_belief/1, 
		 get_belief/1,
		 get_bs/0, 
		 is_belief/1,
		 is_belief/2,
		 stop/0]).

%% ----------------------------
%% gen_server Function Exports
%% ----------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% =========================
%% API Function Definitions
%% =========================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_belief(Belief)->
	gen_server:cast(?MODULE,{add,Belief}).

update_belief(Belief)->
	gen_server:cast(?MODULE, {update, Belief}).

remove_belief(Belief)->
	gen_server:cast(?MODULE, {remove, Belief}).

remove_one_belief(Belief)->
	gen_server:cast(?MODULE, {remove_one, Belief}).

get_belief(Belief)->
	gen_server:call(?MODULE,{get,Belief}).

is_belief(Key)->
	gen_server:call(?MODULE,{is_belief,Key}).

is_belief(Key1,Key2)->
	gen_server:call(?MODULE,{is_belief,Key1,Key2}).

get_bs()->
	gen_server:call(?MODULE,{get_bs}).

stop()->
	gen_server:cast(?MODULE,stop).


%% =================================
%% gen_server Function Definitions
%% =================================

%This function initializes the State of the BeliefStore, which includes or will include all the variables, percepts and beliefs

init([])->
	{ok, #{ last_executed=>
			   {[],[],[]},
		   executing=>
			   {[],[],[]},
		   to_execute=>
			   {[],[],[]},
		   priority=>
			   [{	{[],[]}]}}.

%This set of functions returns data which belongs to the State of the BeliefStore

handle_call({get_bs},_From,State)->
	{reply,State,State};
handle_call({get,Belief},_From,State)->
	{reply,maps:get(Belief,State),State};
handle_call({is_belief,Key},_From,State)->
	{reply,maps:is_key(Key,State),State};
handle_call({is_belief,Key1,Key2},_From,State)->
	case maps:is_key(Key1, State) of
		true->
			List=maps:get(Key1, State),
			case proplists:lookup(Key2, List) of
				none ->
					{reply,false,State};
				_->
					{reply,true,State}
			end;
		false->
			{reply,false,State}
	end.

%This set of functions modifies the State of the BeliefStore

handle_cast(stop,State)->
	{stop,normal,State};
handle_cast({add,{Key,Value}},State)->
	case maps:is_key(Key, State) of
		true->
			case maps:get(Key, State)=:=[] of
				true->
					io:format("Belief added: ~p.~n",[{Key,Value}]),
					{noreply,State#{Key => [Value]}};
				false->
					io:format("Belief added: ~p.~n",[{Key,Value}]),
					{noreply,State#{Key => ordsets:add_element(Value, maps:get(Key, State))}}
			end;
		_->
			io:format("Belief added: ~p.~n",[{Key,Value}]),
			{noreply, maps:put(Key,Value,State)}
	end;
handle_cast({update,{Key,Value}},State)->
	io:format("Belief updated: ~p.~n",[{Key,Value}]),
	{noreply,State#{Key => Value}};
handle_cast({remove,Key},State)->
	io:format("Belief removed: ~p.~n",[Key]),
	{noreply,maps:remove(Key, State)};
handle_cast({remove_one,{Key,Subkey}},State)->
	io:format("Belief removed: ~p.~n",[{Key,Subkey}]),
	{noreply,State#{Key => proplists:delete(Subkey, maps:get(Key, State))}}.

%These functions belong to the gen_server behaviour

handle_info(Info, State) ->     
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          
terminate(_Reason, _State) -> 
    error_logger:info_msg("terminating~n"), 
    ok.                       
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
 