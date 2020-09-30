# Commands

erl -name node4@127.0.0.1 -setcookie secret

N4 = node1:start(4, {node, 'N2@127.0.0.1'}).


N2 = node1:start(2).
N4 = node1:start(4,N2).
N0 = node1:start(0,N2).
N3 = node1:start(3,N2).
N1 = node1:start(1,N2).


register(node, N2).

