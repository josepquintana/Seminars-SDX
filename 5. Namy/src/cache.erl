-module(cache).
-export([lookup/2, add/4, remove/2, purge/1]).


% {Name, {host, Pid}, TTL}


lookup(Name, Cache) ->
	case lists:keyfind(Name, 1, Cache) of
        {Name, Reply, Expire} ->
			Now = erlang:monotonic_time(),
			NowTime = erlang:convert_time_unit(Now, native, second),
			case Expire > NowTime of
				true ->
					Reply;
				false -> 
					invalid
			end;
			
        false -> 
			unknown
	end.
	

add(Name, Expire, Reply, Cache) -> 
	lists:keystore(Name, 1, Cache, {Name, Reply, Expire}).


remove(Name, Cache) ->
	lists:keydelete(Name, 1, Cache).
	

purge(Cache) ->
	NewCache = lists:filter(
		fun
			({_, _, inf}) -> true;				
		
			({Name, _, Expire}) -> 
				Now = erlang:monotonic_time(),
				NowTime = erlang:convert_time_unit(Now, native, second),
				case Expire > NowTime of
					true -> 
						true;
					false ->
						io:format("FILTER: ~w~n", [Name])
				end
				
		end,
			
		Cache),
		
	NewCache.
	