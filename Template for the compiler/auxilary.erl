%% @author Elias Antolinos
%% @doc @todo Module which contains the auxilary functions that allow the tr execution.


-module(auxilary).

%% =============
%% API functions
%% =============

-export([execute_while/1,
		 execute_while/5,
		 execute/1,
		 empty/0,
       add_timeout/3,
		 remember/2,
		 compare_value/4,
		 while_condition/1]).

%% ===============================================
%% Functions that control the execution of actions
%% ===============================================

%% This function controls the execution of durative actions, actions that have to be executed during a 
%certain amount of time or while a condition is true

execute_while([Rule,Fun,Args,true])->
	case check_execution(Rule,Fun,Args) of
		no_action->
			no_action;
		_->
	spawn(?MODULE,execute,[[Rule,Fun,Args]]),
			timer:sleep(5)
	end;
execute_while([Rule,Fun,Args,N])->
	case check_execution(Rule,Fun,Args) of
		no_action->
			no_action;
		_->
	bs:update_belief({to_execute,{Rule,Fun,Args}}),
	writer:writeIt(debug,io_lib:fwrite("Instruction execute_while ~p.~n",[{Rule,Fun,Args}])),
	writer:writeIt(debug,io_lib:fwrite("Executing ~p.~n",[bs:get_belief(executing)])),
	writer:writeIt(debug,io_lib:fwrite("Last executed ~p.~n",[bs:get_belief(last_executed)])),
			writer:writeIt(debug,io_lib:fwrite("To execute ~p.~n",[bs:get_belief(to_execute)])),
	PID=spawn(?MODULE,execute_while,[Rule,Fun,Args,N,timer:tc(?MODULE,execute,[[Rule,Fun,Args]])]),
	writer:writeIt(debug,io_lib:fwrite("Timer ~p.~n",[{PID,Rule,Fun,Args}])),
			bs:update_belief({timer,PID})
	end.
execute_while(Rule,Fun,Args,N,{Time, _Value})  when N>=Time->
	{Time2, Value2} = timer:tc(?MODULE,execute,[[Rule,Fun,Args]]),
	execute_while(Rule,Fun,Args,N,{Time + Time2, Value2});
execute_while(Rule,Fun,Args,_,_) ->
	writer:writeIt(debug,io_lib:fwrite("Terminate ~p.~n",[{self(),Rule,Fun,Args}])),
	tr:Fun({finalize,Rule,Args}).

%% This function controls the execution of discrete actions, which are executed only once

execute([Rule,Fun,Args])->
	case check_execution(Rule,Fun,Args) of
				execute->
	bs:update_belief({to_execute,{Rule,Fun,Args}}),
	writer:writeIt(debug,io_lib:fwrite("Instruction execute execute~p.~n",[{Rule,Fun,Args}])),
	writer:writeIt(debug,io_lib:fwrite("Executing ~p.~n",[bs:get_belief(executing)])),
	writer:writeIt(debug,io_lib:fwrite("Last executed ~p.~n",[bs:get_belief(last_executed)])),
	writer:writeIt(debug,io_lib:fwrite("To execute ~p.~n",[bs:get_belief(to_execute)])),
				tr:Fun({start,Rule,Args}),
	bs:update_belief({to_execute,{[],[],[]}});
		
		update->
	bs:update_belief({to_execute,{Rule,Fun,Args}}),
	writer:writeIt(debug,io_lib:fwrite("Instruction execute update ~p.~n",[{Rule,Fun,Args}])),
	writer:writeIt(debug,io_lib:fwrite("Executing ~p.~n",[bs:get_belief(executing)])),
	writer:writeIt(debug,io_lib:fwrite("Last executed ~p.~n",[bs:get_belief(last_executed)])),
	writer:writeIt(debug,io_lib:fwrite("To execute ~p.~n",[bs:get_belief(to_execute)])),
	{Rule2,Fun2,Args2}=bs:get_belief(executing),
			case bs:get_belief(timer)=:=[] of
				false->
		exit(bs:get_belief(timer),kill),
	writer:writeIt(debug,io_lib:fwrite("Kill ~p.~n",[bs:get_belief(timer)])),
			bs:update_belief({timer,[]});
				_->
					no_action
			end,
			tr:Fun({update,Rule,Args}),
	bs:update_belief({to_execute,{[],[],[]}});
				execute_priority->
	bs:update_belief({to_execute,{Rule,Fun,Args}}),
	writer:writeIt(debug,io_lib:fwrite("Instruction execute priority~p.~n",[{Rule,Fun,Args}])),
	writer:writeIt(debug,io_lib:fwrite("Executing ~p.~n",[bs:get_belief(executing)])),
	writer:writeIt(debug,io_lib:fwrite("Last executed ~p.~n",[bs:get_belief(last_executed)])),
	writer:writeIt(debug,io_lib:fwrite("To execute ~p.~n",[bs:get_belief(to_execute)])),
			
			{Rule2,Fun2,Args2}=bs:get_belief(executing),
			case bs:get_belief(timer)=:=[] of
				false->
		exit(bs:get_belief(timer),kill),
	writer:writeIt(debug,io_lib:fwrite("Kill ~p.~n",[bs:get_belief(timer)])),
			bs:update_belief({timer,[]});
				_->
					no_action
			end,
		tr:Fun2({finalize,Rule2,Args2}),
			tr:Fun({start,Rule,Args}),
			bs:update_belief({to_execute,{[],[],[]}});
				_->
			no_action
	end.

%% This function allows to keep one condition as true for a certain amount of time, even if it is not true

until_condition([Fun,Args,N])->
	bs:add_belief({until,Fun}),
	until_condition(Fun,Args,N,timer:tc(tr,Fun,Args)).
until_condition(Fun,Args,N,{Time, _Value}) when N>=Time->
	 {Time2, Value2} = timer:tc(tr,Fun,Args),
	 until_condition(Fun,Args,N,{Time + Time2, Value2});
until_condition(_,_,_,_)->
	bs:remove_belief(until).

%% This function is used to maintain for a given time the execution of a rule, despite of the evaluation of its condition. 

while_condition([Rule,Fun,Args,N])->
	PID=spawn(?MODULE,while_condition,[Rule,Fun,Args,N,timer:tc(?MODULE,empty,[])]),
	bs:update_belief({while_timer,{Rule,PID}}).
while_condition(Rule,Fun,Args,N,{Time, _Value}) when N>=Time->
	 {Time2, Value2} = timer:tc(?MODULE,empty,[]),
	 while_condition(Rule,Fun,Args,N,{Time + Time2, Value2});
while_condition(_,_,_,_,_)->
	bs:remove_belief(while_timer).

%% This function ends the execution of the previous functions due to one condition with higher priority being true

kill_while(Rule)->
		case bs:is_belief(while_timer) of
		true->
	{RuleWhile,PID}=bs:get_belief(while_timer),
			case priority(get_priority(Rule,bs:get_belief(priority)),get_priority(RuleWhile,bs:get_belief(priority))) of
						more_priority->
	writer:writeIt(debug,io_lib:fwrite("Kill while timer ~p.~n",[PID])),
			bs:remove_belief(while_timer),
					exit(PID,kill);
				_->
					no_action
			end;
		_->
			no_action
	end.

%% This function ends the execution of a timeout

kill_timeout(Rule)->
	
	case bs:is_belief(timeout_timer) of
		true->
			{RuleWhile,PID}=bs:get_belief(timeout_timer),
			case priority(get_priority(Rule,bs:get_belief(priority)),get_priority(RuleWhile,bs:get_belief(priority))) of
				
				more_priority->
					writer:writeIt(debug,io_lib:fwrite("Kill timeout timer ~p.~n",[PID])),
					bs:remove_belief(timeout_timer),
					exit(PID,kill);
				_->
					no_action
			end;
		_->
			no_action
	end.

%% This function checks if the action that is going to be triggered should be executed in terms of priority

check_execution(Rule,Fun,Args)->
	{ExecRule,ExecFun,ExecArgs}=bs:get_belief(executing),
	{LastRule,LastFun,LastArgs}=bs:get_belief(last_executed),
	{NextRule,NextFun,NextArgs}=bs:get_belief(to_execute),
		kill_while(Rule),
      kill_timeout(Rule),
		case ({ExecRule=:=[],priority(get_priority(Rule,bs:get_belief(priority)),get_priority(ExecRule,bs:get_belief(priority))),Fun=:=ExecFun,Args=:=ExecArgs,Fun=:=LastFun,Args=:=LastArgs}) of
		{true,_,_,_,true,true}->
			no_action;
		{true,_,_,_,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of
				{false, less_priority}->
					execute;
				{true,_}->
					execute;
				{_,_}->
					no_action
			end;
		{false,more_priority,false,_,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of
				{false, less_priority}->
					execute_priority;
				{true,_}->
					execute_priority;
				{_,_}->
					no_action
			end;
	{false,more_priority,true,false,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of
				{false, less_priority}->
					update;
				{true,_}->
					update;
				{_,_}->
					no_action
			end;
		{_,_,_,_,_,_}->
			no_action
	end.

%% This function keeps a belief in the BeliefStore during a certain amount of time

remember(Belief,Time)->
	bs:add_belief(Belief),
	remember(Belief,Time,timer:tc(?MODULE,empty,[])).
remember(Belief,TimeWanted,{Time,_Value}) when TimeWanted>Time ->
   {Time2, Value2} = timer:tc(?MODULE,empty,[]),
remember(Belief,Time,{Time+Time2,Value2});
remember(Belief,_,_)->
	bs:remove_belief(Belief).

%% This function is used to trigger a timeout in such a way that a belief is added to the BS 
%% if the action has not been executed before the deadline.
add_timeout(Belief,Rule,Time)->
	PID=spawn(?MODULE,add_timeout,[Belief,Rule,Time,timer:tc(?MODULE,empty,[])]),
	bs:add_belief({timeout_timer,{Rule,PID}}).
add_timeout(Belief,Rule,TimeWanted,{Time,_Value}) when TimeWanted>Time ->
   {Time2, Value2} = timer:tc(?MODULE,empty,[]),
   add_timeout(Belief,Rule,Time,{Time+Time2,Value2});
add_timeout(Belief,_,_,_)->
	bs:add_belief({timeout,Belief}).

%% ==================
%% Priority functions
%% ==================

%% This set of functions are used to calculate the priority of a certain Rule, which consists of the set of conditions 
%that index a certain action, normally {Function,SubFunction}

get_priority({MainFun,SubFun},ListOfLists)->
	get_max_priority({MainFun,SubFun},ListOfLists,1);
get_priority(_,ListOfLists)->
	get_max_priority({[],[]},ListOfLists,1).
	get_max_priority(_,[],_)->
	not_found;
get_max_priority({MainFun,SubFun},[{MainFun,SubList}|_],MaxPriority)->
	get_sub_priority(SubFun,SubList,MaxPriority,1);
get_max_priority({MainFun,SubFun},[_|List],Priority)->
	get_max_priority({MainFun,SubFun},List,Priority+1).
get_sub_priority(_,[],MaxPriority,_)->
	{MaxPriority,not_found};
get_sub_priority(SubFun,[SubFun|_],MaxPriority,SubPriority)->
	{MaxPriority,SubPriority};
get_sub_priority(SubFun,[_|List],MaxPriority,SubPriority)->
	get_sub_priority(SubFun,List,MaxPriority,SubPriority+1).
priority({A,B},{C,D}) when A<C ->	more_priority;
priority({A,B},{C,D}) when A=:=C, B<D ->more_priority;
priority({A,B},{C,D}) when A=:=C, B=:=D -> same_priority;
priority(_,_)->less_priority.

%% ===============
%% Other functions
%% ===============

%% This function compares two values which belong to two separate lists and returns true if the condition is true. 
%It compares if the value A is greater, equal or minor than value B.

compare_value(Key,List,Mode,Value)->
	case bs:is_belief(Key) of
		true->
			case Mode of
				minor ->
				proplists:get_value(Key, List) < Value;
				minor_equal ->
				proplists:get_value(Key, List) =< Value;
				mayor ->
				proplists:get_value(Key, List) > Value;
				mayor_equal ->
				proplists:get_value(Key, List) >= Value;
				equal ->
				proplists:get_value(Key, List) =:= Value
			end;
		false->
			false
end.