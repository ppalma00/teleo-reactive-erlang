%% @author Elias Antolinos
%% @doc @todo Module to write a log file.


-module(writer).

%% ===============
%% API functions
%% ===============
-export([writeIt/2]).


%% ===================
%% Internal functions
%% ===================

%This functions has two modes: one to debug, where more detailed information about the execution of the program is recorded; 
% and a normal mode, where only the basic information is recorded.

writeIt(debug,Sequence)->
	{Mode,File}=bs:get_belief(writer),
	case Mode=:=debug of
		true->
			write(Sequence,string:concat(File,"_debug.dat"));
		false->
			no_action
	end;

writeIt(normal,Sequence)->
	{Mode,File}=bs:get_belief(writer),
	case Mode=:=debug of
		true->
			write(Sequence,string:concat(File,"_debug.dat"));
		false->
			write(Sequence,string:concat(File,".dat"))
	end.

%Extra functions

write(Sequence,File)->
	{ok,IoDevice}=file:open(File,[append]),
	file:write(IoDevice, string:concat(format_utc_timestamp(), Sequence)).

format_utc_timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}} =
calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
    "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w ",
    [Day,Mstr,Year,Hour+2 rem 24,Minute,Second,Micro]).