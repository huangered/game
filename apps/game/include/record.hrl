-record(sock, {socket, transfer}).

%% the record to store user, account, player, world info
-record(user_detail, {user_id, player_id, world_id, player_proc, world_proc}).
-record(user_socket, {user_id, socket, transfer}).

-record(user, {id, username, password}).

-record(player, {x, y, hp, mp}).

