%% @author Usuario_PC
%% @doc @todo Add description to tr.


-module(tr).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/2,
		 init/4,
		 go/2,
		 communicating_collect_bottles/0,
		 move/1,
		 turn/1,
		 close_gripper/1,
		 open_gripper/1,
		 handle_message/0,
		 add_belief/1,
		 remove_belief/1,
		 update_belief/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
test(OthrAg,Name)->
	PID=spawn(OthrAg,tr,handle_message,[]),
	init(5,PID,debug,Name).

init(Num,OthrAg,Mode,File)->
	bs:start_link(),
	bs:add_belief({writer,{Mode,File}}),
	spawn(tr,go,[Num,OthrAg]).

go(Num,OthrAg)->
	bs:add_belief({total,Num}),
	bs:add_belief({othrAg,OthrAg}),
	communicating_collect_bottles(). 

communicating_collect_bottles()->
	executor:update_execution(communicating_collect_bottles),
	case {bs:get_belief(collected)+bs:get_belief(other_collected)>=bs:get_belief(total), bs:is_belief(holding) andalso next_to(drop), bs:is_belief(while_timer), bs:is_belief(holding)} of
		
		{true,_,_,_}->
			io:format("~p cans collected, task finished.~n",[bs:get_belief(total)]),
			writer:writeIt(normal, io_lib:fwrite("~p cans collected, task finished.~n",[bs:get_belief(total)]));
		
		{_,true,_,true}-> 
			drop_and_leave();
		
		{_,_,true,_}->
			drop_and_leave();
		
		{_,true,_,_} -> 
			get_to_drop();
		
		{_,_,_,_} -> 
			get_bottle()
	
	end.

drop_and_leave()->
	executor:update_execution(drop_and_leave),
	case bs:is_belief(while_timer) of
		true->
			ok;
		_->
			bs:add_belief({while_timer,[]}),
			executor:while_condition([{communicating_collect_bottles,holding_over_drop},3000])
	end,
	
	case bs:is_belief(gripper_open) of
		
		true -> 
			leave_drop();
		
		_ -> 
			executor:execute([{drop_and_leave,true},open_gripper,[]]),
			update_and_communicate_count(bs:get_belief(othrAg))
				
	end.

update_and_communicate_count(OthrAg)->
	bs:update_belief({collected,bs:get_belief(collected)+1}),
	OthrAg ! {count,bs:get_belief(collected)},
	communicating_collect_bottles().

leave_drop()->
	executor:update_execution(leave_drop),
	case  {bs:is_belief(see,drop),bs:is_belief(see,robot)} of
		
		{true,_} -> 
			executor:execute_while([{leave_drop,see_drop},turn,{left,0.8},true]),
			communicating_collect_bottles();
		
		{_,true} -> 
			executor:execute_while([{leave_drop,see_robot},turn,{right,0.8},true]),
			communicating_collect_bottles();
		
		{_,_} -> 
			executor:execute_while([{leave_drop,not_see},move,{1.0},true]),
			communicating_collect_bottles()
	
	end.

next_to(Th)->

	case executor:compare_value(Th, bs:get_belief(see), minor, 15) of
		true->
			true;
		_->
			false
	end.
	

get_to_drop()->
	executor:update_execution(get_to_drop),
	case {bs:is_belief(over_drop), next_to(drop), executor:compare_value(drop, bs:get_belief(see), mayor, 40)} of
		
		{true,_,_} -> 
			communicating_collect_bottles(); 
		
		{_,true,_} -> 
			executor:execute_while([{get_to_drop,next_drop},move,{1.0},true]),
			communicating_collect_bottles();
		{_,_,true}->
			executor:execute_while([{get_to_drop,see_drop},move,{2.0},true]),
			communicating_collect_bottles();
		{_,_,_} ->
			executor:execute_while([{get_to_drop,true},turn,{left,1.0},2000]),
			executor:execute_while([{get_to_drop,true},move,{1.0},2000]),
			communicating_collect_bottles()
	
	end.

get_bottle()->
	executor:update_execution(get_bottle),
	case {bs:is_belief(holding),executor:compare_value(touching, bs:get_bs(), equal, centre) andalso bs:is_belief(gripper_open),bs:is_belief(touching) andalso bs:is_belief(gripper_open), next_to(bottle), bs:is_belief(see,bottle)} of
		
		{true,_,_,_,_} -> 
			communicating_collect_bottles();
		
		{_,true,_,_,_} -> 
			executor:execute([{get_bottle,touching_centre_gripper_open},close_gripper,[]]),
			communicating_collect_bottles();
		
		{_,_,true,_,_} -> 
			executor:execute_while([{get_bottle,touching_gripper_open},turn,{left,0.2},true]),
			communicating_collect_bottles();
		{_,_,_,true,_}->
			executor:execute([{get_bottle,next_bottle},open_gripper,[]]),
			communicating_collect_bottles();
		{_,_,_,_,true} ->
			executor:execute_while([{get_bottle,see_bottle},move,{1.5},true]),
			communicating_collect_bottles();		
 		{_,_,_,_,_} ->
			executor:execute_while([{get_bottle,true},turn,{left,1.0},2800]),
			executor:execute_while([{get_bottle,true},move,{1.0},2000]),
			communicating_collect_bottles()
	
	end.

handle_message()->
	receive
		{count,Num}->
			bs:update_belief({other_collected,Num}),
			handle_message();
		{_,_}->
			handle_message()
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%			ACCIONES AUXILIARES	DEL TR		%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%Implementación de las acciones del tr

move({Atom,Rule,Args})->
	
	case Atom of
		start->	
			bs:update_belief({executing,{Rule,move,Args}}),
			writer:writeIt(normal,io_lib:fwrite("Begin move ~p.~n",[Args]));
		finalize->
			bs:update_belief({last_executed,{Rule,move,Args}}),
			bs:update_belief({executing,{[],[],[]}}),
			writer:writeIt(normal,io_lib:fwrite("End move ~p.~n",[Args]));
		update->
			bs:update_belief({last_executed,bs:get_belief(executing)}),
			bs:update_belief({executing,{Rule,move,Args}}),
			writer:writeIt(normal,io_lib:fwrite("Move updated to ~p.~n",[Args]))
	end.


turn({Atom,Rule,Args})->

	case Atom of	
		start->
			bs:update_belief({executing,{Rule,turn,Args}}),
			writer:writeIt(normal,io_lib:fwrite("Begin turn ~p.~n",[Args]));
		finalize->
			bs:update_belief({last_executed,{Rule,turn,Args}}),
			bs:update_belief({executing,{[],[],[]}}),
			writer:writeIt(normal,io_lib:fwrite("End turn ~p.~n",[Args]));
		update->
			bs:update_belief({last_executed,bs:get_belief(executing)}),
			bs:update_belief({executing,{Rule,turn,Args}}),
			writer:writeIt(normal,io_lib:fwrite("Turn updated to ~p.~n",[Args]))
	end.

close_gripper({_,Rule,_})->
	bs:update_belief({last_executed,{Rule,close_gripper,[]}}),
	writer:writeIt(normal,io_lib:fwrite("Gripper closed.~n",[])).

open_gripper({_,Rule,_})->
	bs:update_belief({last_executed,{Rule,open_gripper,[]}}),
	writer:writeIt(normal,io_lib:fwrite("Gripper opened.~n",[])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%			GOD FUNCTIONS			  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Estas funciones me permiten modificar la BS mientras se está ejecutando el tr, sólo para testing, no formaría parte del tr

add_belief(Belief)->
	bs:add_belief(Belief).

remove_belief(Belief)->
	bs:remove_belief(Belief).

update_belief(Belief)->
	bs:update_belief(Belief).