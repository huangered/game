-record(sock, {socket, transport}).

%% the record to store user, account, player, world info
-record(user_detail, {user_id, player_id, world_id, player_proc, world_proc}).

-record(user, {id, username, password}).

-record(player, {x, y, hp, mp}).

