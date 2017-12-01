%% @author Usuario_PC
%% @doc @todo Add description to executor.


-module(executor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute_while/1,
		 execute/1,
		 while_condition/1,
		 remember/2,
		 update_execution/1,
		 compare_value/4]).


%% ====================================================================
%% Internal functions
%% ====================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%		CONTROL DE ACCIONES DURATIVAS/DISCRETAS 	%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Con estas funciones defino como se ejecutan las acciones discretas, durativas y la implementación del while

execute_while([Rule,Fun,Args,true])->
	case check_execution(Rule,Fun,Args) of
		no_action->
			no_action;
		_->
			spawn(?MODULE,execute,[[Rule,Fun,Args]]),
			timer:sleep(5)
	end;

execute_while([Rule,Fun,Args,Time])->
	case check_execution(Rule,Fun,Args) of
		no_action->
			no_action;
		_->
			bs:update_belief({to_execute,{Rule,Fun,Args}}),
			writer:writeIt(debug,io_lib:fwrite("Instruction execute_while ~p.~n",[{Rule,Fun,Args}])),
			writer:writeIt(debug,io_lib:fwrite("Executing ~p.~n",[bs:get_belief(executing)])),
			writer:writeIt(debug,io_lib:fwrite("Last executed ~p.~n",[bs:get_belief(last_executed)])),
			writer:writeIt(debug,io_lib:fwrite("To execute ~p.~n",[bs:get_belief(to_execute)])),
			
			execute([Rule,Fun,Args]),
			{ok,TRef}=timer:apply_after(Time, tr, Fun, [{finalize,Rule,Args}]),
			writer:writeIt(debug,io_lib:fwrite("Timer ~p.~n",[{TRef,Rule,Fun,Args}])),
			bs:update_belief({timer,TRef})
	end.
			
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
					timer:cancel(bs:get_belief(timer)),
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
					timer:cancel(bs:get_belief(timer)),
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

while_condition([Rule,Time])->
	{ok,TRef}=timer:apply_after(Time, bs, remove_belief, [while_timer]),
	bs:update_belief({while_timer,{Rule,TRef}}).

kill_while(Rule)->	
	case bs:is_belief(while_timer) of
		true->
			{RuleWhile,TRef}=bs:get_belief(while_timer),
			case priority(get_priority(Rule,bs:get_belief(priority)),get_priority(RuleWhile,bs:get_belief(priority))) of
				
				more_priority->
					writer:writeIt(debug,io_lib:fwrite("Kill while timer ~p.~n",[TRef])),
					bs:remove_belief(while_timer),
					timer:cancel(TRef);
				_->
					no_action
			end;
		_->
			no_action
	end.
	

check_execution(Rule,Fun,Args)->
	{ExecRule,ExecFun,ExecArgs}=bs:get_belief(executing),
	{LastRule,LastFun,LastArgs}=bs:get_belief(last_executed),
	{NextRule,NextFun,NextArgs}=bs:get_belief(to_execute),
	
	kill_while(Rule),
	
	case ({ExecRule=:=[],priority(get_priority(Rule,bs:get_belief(priority)),get_priority(ExecRule,bs:get_belief(priority))),Fun=:=ExecFun,Args=:=ExecArgs,Fun=:=LastFun,Args=:=LastArgs}) of
		{true,_,_,_,true,true}->
			no_action;
		{true,_,_,_,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of	
				{true,_}->
					execute;
				{_, less_priority}->
					execute;
				{_,_}->
					no_action
			end;
		{false,more_priority,false,_,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of
				{true,_}->
					execute_priority;
				{_, less_priority}->
					execute_priority;
				{_,_}->
					no_action
			end;
		{false,more_priority,true,false,_,_}->
			case ({NextRule=:=Rule andalso NextFun=:=Fun andalso NextArgs=:=Args, priority(get_priority(NextRule,bs:get_belief(priority)),get_priority(Rule,bs:get_belief(priority)))}) of	
				{true,_}->
					update;
				{_, less_priority}->
					update;
				{_,_}->
					no_action
			end;
		{_,_,_,_,_,_}->
			no_action
	end.
		
remember({Belief,Args},Time)->
	bs:add_belief({Belief,Args}),
	timer:apply_after(Time, bs, remove_belief, [Belief]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%			FUNCIONES PARA PRIORIDAD		%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

update_execution(Rule)->
	{ExecRule,Fun,Args}=bs:get_belief(executing),
	case ExecRule=:=[] of
		true->
			MainRule=[];
		_->
			{MainRule,_}=ExecRule
	end,
	case priority(get_priority({Rule,[]},bs:get_belief(priority)),get_priority({MainRule,[]},bs:get_belief(priority))) of
		less_priority->
			bs:update_belief({executing,{{[],[]},Fun,Args}});
		_->
			no_action
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% OTHER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_value(Key,KeyList,Mode,Value)->
	case KeyList=:=all of
		true->
			List=bs:get_bs(),
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
				_->
					false
			end;
		false->
			case bs:is_belief(KeyList,Key) of
				true->
					List=bs:get_belief(KeyList),
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
				_->
					false
			end
	end.
