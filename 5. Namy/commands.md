# ROOT SERVER

erl -name root@127.0.0.1 -setcookie dns

server:start().

# EDU SERVER

erl -name edu@127.0.0.1 -setcookie dns

server:start(edu, {server, 'root@127.0.0.1'}).

# UPC SERVER

erl -name upc@127.0.0.1 -setcookie dns

server:start(upc, {server, 'edu@127.0.0.1'}).

# HOSTS SERVER

erl -name hosts@127.0.0.1 -setcookie dns

### CREATE

host:start(www, www, {server, 'upc@127.0.0.1'}).

host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

host:start(vpn, vpn, {server, 'upc@127.0.0.1'}).

### REMOVE

host:stop(www).
host:stop(ftp).
host:stop(vpn).

# CLIENTS

erl -name client@127.0.0.1 -setcookie dns

resolver:start({server, 'root@127.0.0.1'}).

namy:ping([www,upc,edu], resolver).

### STOP

resolver:stop().

# TTL UPDATE

{server, 'upc@127.0.0.1'} ! {ttl, 60}.

{server, 'edu@127.0.0.1'} ! {ttl, 60}.

{server, 'root@127.0.0.1'} ! {ttl, 60}.

