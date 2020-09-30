-module(node4).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),    
	Next = nil,
	Store = Replica = storage:create(),
    node(MyKey, Predecessor, Successor, Next, Store, Replica).

connect(MyKey, nil) ->
    {ok, {MyKey, nil, self()}};    %% TODO: ADD SOME CODE
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
			Ref = monit(PeerPid),
            {ok, {Skey, Ref, PeerPid}}    %% TODO: ADD SOME CODE
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Next, Store, Replica) ->
    receive 
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {notify, NewPeer} ->
            {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Successor, Store),
            node(MyKey, NewPredecessor, Successor, Next, NewStore, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {status, Pred, Nx} ->
            {NewSuccessor, NewNext} = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, NewNext, Store, Replica);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store, Replica),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey|Nodes], T, Successor, Store, Replica),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Next, Added, Replica);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		{handover, Elements} ->
			Merged = merge(Store, Elements, Successor),
			node(MyKey, Predecessor, Successor, Next, Merged, Replica);
		{'DOWN', Ref, process, _, _} ->
			{NewPred, NewSucc, NewNext, NewStore, NewReplica} = down(Ref, Predecessor, Successor, Next, Store, Replica),
			node(MyKey, NewPred, NewSucc, NewNext, NewStore, NewReplica);
		{replicate, Key, Value} ->
			NewReplica = add_replica(Key, Value, Replica),
			node(MyKey, Predecessor, Successor, Next, Store, NewReplica);
		{pushreplica, NewReplica} ->
			node(MyKey, Predecessor, Successor, Next, Store, NewReplica)
			
   end.

stabilize(Pred, Nx, MyKey, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
      nil ->
          Spid ! {notify, {MyKey, self()}}, %% TODO: ADD SOME CODE
          {Successor, Nx};
      {MyKey, _} ->
          {Successor, Nx};
      {Skey, _} ->
          Spid ! {notify, {MyKey, self()}}, %% TODO: ADD SOME CODE
          {Successor, Nx};
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    self() ! stabilize, %% TODO: ADD SOME CODE
					demonit(Sref),
					Xref = monit(Xpid),
					NewPred = {Xkey, Xref, Xpid},
					NewNx = {Skey, Spid},
                    {NewPred, NewNx}; %% TODO: ADD SOME CODE 
                false ->
                    Spid ! {notify, {MyKey, self()}}, %% TODO: ADD SOME CODE
                    {Successor, Nx}
            end
    end.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Successor, Store) ->
	{_, _, Spid} = Successor,
    case Predecessor of
        nil ->
            Keep = handover(Store, MyKey, Nkey, Npid, Spid),
			Nref = monit(Npid),
			NewPredecessor = {Nkey, Nref, Npid},
			{NewPredecessor, Keep}; %% TODO: ADD SOME CODE
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->    
					Keep = handover(Store, MyKey, Nkey, Npid, Spid), %% TODO: ADD SOME CODE
					demonit(Pref),
					Nref = monit(Npid),
					NewPredecessor = {Nkey, Nref, Npid},
					{NewPredecessor, Keep}; %% TODO: ADD SOME CODE 
                false -> 
                    {Predecessor, Store}
            end
    end.

create_probe(MyKey, {_, _, Spid}, Store, Replica) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Create probe ~w! Store: ~w Replica: ~w~n", [MyKey, Store, Replica]).
	
remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2-T, native, millisecond),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(RefKey, Nodes, T, {_, _, Spid}, Store, Replica) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w! Store: ~w Replica: ~w~n", [RefKey, Store, Replica]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key, Pkey, MyKey) of %% TODO: ADD SOME CODE
		true ->
			Added = storage:add(Key, Value, Store), %% TODO: ADD SOME CODE
			Spid ! {replicate, Key, Value},
			Client ! {Qref, ok},
			Added;
		false ->
			Spid ! {add, Key, Value, Qref, Client}, %% TODO: ADD SOME CODE
			Store
	end.

lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key, Pkey, MyKey) of %% TODO: ADD SOME CODE
		true ->
			Result = storage:lookup(Key, Store), %% TODO: ADD SOME CODE
			Client ! {Qref, Result};
		false ->
			Spid ! {lookup, Key, Qref, Client} %% TODO: ADD SOME CODE
	end.
	
handover(Store, MyKey, Nkey, Npid, Spid) ->
	{Keep, Leave} = storage:split(MyKey, Nkey, Store),
	Npid ! {handover, Leave},
	Spid ! {pushreplica, Keep},
	Keep.	
	
monit(Pid) ->
	erlang:monitor(process, Pid).

demonit(nil) ->
	ok;
	
demonit(MonitorRef) ->
	erlang:demonitor(MonitorRef, [flush]).
	
down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
	NewStore = merge(Store, Replica, Successor),
	NewReplica = storage:create(),
	{nil, Successor, Next, NewStore, NewReplica};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
	Nref = monit(Npid), %% TODO: ADD SOME CODE
	self() ! stabilize, %% TODO: ADD SOME CODE
	{Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.

add_replica(Key, Value, Replica) ->
	NewReplica = storage:add(Key, Value, Replica),
	NewReplica.
	
merge(Store, Elements, {_, _, Spid}) ->
	Merged = storage:merge(Store, Elements),
	Spid ! {pushreplica, Merged},
	Merged.

	