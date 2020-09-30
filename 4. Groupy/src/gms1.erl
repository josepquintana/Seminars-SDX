-module(gms1).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(), 
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            slave(Name, Master, Leader, Slaves)
    end.

leader(Name, Master, Slaves) ->    
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, Msg}, Slaves),  %% TODO: COMPLETE
            Master ! {deliver, Msg}, %% TODO: ADD SOME CODE
            leader(Name, Master, Slaves);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name, {view, self(), NewSlaves}, NewSlaves),  %% TODO: COMPLETE
            leader(Name, Master, NewSlaves);  %% TODO: COMPLETE
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    
bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

slave(Name, Master, Leader, Slaves) ->    
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg}, %% TODO: ADD SOME CODE
            slave(Name, Master, Leader, Slaves);
        {join, Peer} ->
            Leader ! {join, Peer}, %% TODO: ADD SOME CODE
            slave(Name, Master, Leader, Slaves);
        {msg, Msg} ->
            Master ! {deliver, Msg}, %% TODO: ADD SOME CODE
            slave(Name, Master, Leader, Slaves);
        {view, Leader, NewSlaves} ->
            slave(Name, Master, Leader, NewSlaves);  %% TODO: COMPLETE
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.
