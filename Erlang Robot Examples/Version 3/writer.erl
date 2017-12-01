%% @author Usuario_PC
%% @doc @todo Add description to writer.


-module(writer).

%% ====================================================================
%% API functions
%% ====================================================================
-export([writeIt/2]).


%% ====================================================================
%% Internal functions
%% ====================================================================
writeIt(debug,Sequence)->
	{Mode,File}=bs:get_belief(writer),
	case Mode=:=debug of
		true->
			write(Sequence,string:concat(File,"_debug.dat"));
		_->
			no_action
	end;

writeIt(normal,Sequence)->
	{Mode,File}=bs:get_belief(writer),
	case Mode=:=debug of
		true->
			write(Sequence,string:concat(File,"_debug.dat"));
		_->
			write(Sequence,string:concat(File,".dat"))
	end.
	
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
    [Day,Mstr,Year,Hour+2,Minute,Second,Micro]).