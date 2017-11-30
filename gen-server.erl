%% This function initializes the State of the BeliefStore, which includes or will include all the variables, percepts and beliefs

init([])->
	{ok, #{ last_executed=>
			   {[],[],[]},
		   executing=>
			   {[],[],[]},
		   to_execute=>
			   {[],[],[]},
		   priority=>
			   [{	{[],[]}]}}.

%% This set of functions returns data which belongs to the State of the BeliefStore

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

%% This set of functions modifies the State of the BeliefStore

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

%% These functions belong to the gen_server behaviour

handle_info(Info, State) ->     
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          
terminate(_Reason, _State) -> 
    error_logger:info_msg("terminating~n"), 
    ok.                       
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
