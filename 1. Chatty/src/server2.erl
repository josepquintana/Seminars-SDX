-module(server2).
%% Exported Functions
-export([start/0, start/1]).

start() ->
    ServerPid = spawn(fun() -> init_server() end),
    register(myserver, ServerPid).

start(BootServer) ->
    ServerPid = spawn(fun() -> init_server(BootServer) end),
    register(myserver, ServerPid).

init_server() ->
    process_requests([], [self()]).

init_server(BootServer) ->
    BootServer ! {server_join_req, self()},
    process_requests([], []).

process_requests(Clients, Servers) ->
    receive
        %% Messages between client and server
        {client_join_req, Name, From} ->
            NewClients = [...|Clients],  %% TODO: COMPLETE
            broadcast(..., {join, Name}),  %% TODO: COMPLETE
            process_requests(..., ...);  %% TODO: COMPLETE
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(..., ...),  %% TODO: COMPLETE
            broadcast(..., {leave, Name}),  %% TODO: COMPLETE
            From ! exit,
            process_requests(..., ...);  %% TODO: COMPLETE
        {send, Name, Text} ->
            broadcast(Servers, ...),  %% TODO: COMPLETE
            process_requests(Clients, Servers);
        
        %% Messages between servers
        disconnect ->
            NewServers = lists:delete(self(), ...),  %% TODO: COMPLETE
            broadcast(..., {update_servers, ...}),  %% TODO: COMPLETE
            unregister(myserver);
        {server_join_req, From} ->
            NewServers = [...|Servers],  %% TODO: COMPLETE
            broadcast(..., {update_servers, NewServers}),  %% TODO: COMPLETE
            process_requests(Clients, ...);  %% TODO: COMPLETE
        {update_servers, NewServers} ->
            io:format("[SERVER UPDATE] ~w~n", [NewServers]),
            process_requests(Clients, ...);  %% TODO: COMPLETE
            
        RelayMessage -> %% Whatever other message is relayed to its clients
            broadcast(Clients, RelayMessage),
            process_requests(Clients, Servers)
    end.

broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
