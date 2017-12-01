-module(tr).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0,
		 go/3,
		 move/1,
		 turn/1,
		 close_gripper/1,
		 open_gripper/1,
		 add_belief/1,
		 remove_belief/1,
		 update_belief/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
test()->
	spawn(tr,go,[normal,"prueba_normal",5]).

go(Mode,File,Num)->
	bs:start_link(),
	bs:add_belief({writer,{Mode,File}}),
	bs:add_belief({goal,Num}),
	collect_bottles(). 

collect_bottles()->
	executor:update_execution(collect_bottles),
	case {bs:get_belief(collected)>=bs:get_belief(goal), bs:is_belief(holding), bs:is_belief(while_timer), bs:is_belief(over_drop)} of
		
		{true,_,_,_}->
			io:format("~p cans collected, task finished.~n",[bs:get_belief(collected)]),
			writer:writeIt(normal, io_lib:fwrite("~p cans collected, task finished.~n",[bs:get_belief(collected)]));
		
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
			executor:while_condition([{collect_bottles,holding_over_drop},3000])
	end,
	
	case bs:is_belief(gripper_open) of
		
		true -> 
			leave_drop();
		
		_ -> 
			executor:execute([{drop_and_leave,true},open_gripper,[]]),
			bs:update_belief({collected,bs:get_belief(collected)+1}),
			collect_bottles()
	
	end.

leave_drop()->
	executor:update_execution(leave_drop),
	case  bs:get_belief(see) of
		
		drop -> 
			executor:execute_while([{leave_drop,see_drop},turn,{left,0.8},true]),
			collect_bottles();	
		robot -> 
			executor:execute_while([{leave_drop,see_robot},turn,{right,0.8},true]),
			collect_bottles();		
		_ -> 
			executor:execute_while([{leave_drop,not_see},move,{1.0},true]),
			collect_bottles()	
	end.

get_to_drop()->
	executor:update_execution(get_to_drop),
	case {bs:is_belief(over_drop),bs:get_belief(see)} of
		
		{true, _} -> 
			collect_bottles(); 
		
		{_, drop} -> 
			executor:execute_while([{get_to_drop,see_drop},move,{1.5},true]),
			collect_bottles();
		
		{_, _} ->
			executor:execute_while([{get_to_drop,true},turn,{left,1.0},2000]),
			executor:execute_while([{get_to_drop,true},move,{1.0},2000]),
			collect_bottles()
	
	end.

get_bottle()->
	executor:update_execution(get_bottle),
	case {bs:is_belief(holding),bs:is_belief(touching),bs:is_belief(gripper_open),bs:get_belief(see)} of
		
		{true, _, _, _} -> 
			collect_bottles();
		
		{_, true, true, _} -> 
			executor:execute([{get_bottle,touching_gripper_open},close_gripper,[]]),
			collect_bottles();
		
		{_, true, _, _} -> 
			executor:execute([{get_bottle,touching},open_gripper,[]]),
			collect_bottles();
		
		{_, _, _, bottle} ->
			executor:execute_while([{get_bottle,see_bottle},move,{1.5},true]),
			collect_bottles();
		
 		{_, _, _, _} ->
			executor:execute_while([{get_bottle,true},turn,{left,1.0},2800]),
			executor:execute_while([{get_bottle,true},move,{1.0},2000]),
			collect_bottles()
	
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
	bs:remove_belief({gripper_open,[]}),%
	writer:writeIt(normal,io_lib:fwrite("Gripper closed.~n",[])).

open_gripper({_,Rule,_})->
	bs:update_belief({last_executed,{Rule,open_gripper,[]}}),
	bs:add_belief({gripper_open,[]}),%
	bs:remove_belief(holding),%
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

