-module(game_db).
-export([open/0, 
         close/1,
         select_user/3,
         select_users/1,
         list_player/2]).

open() ->
    {ok, Conn}=epgsql:connect("localhost", "huangered", "",
                              [
                               { database,"huangered",
                                 timeout, 5000
                               }]),
    Conn.

close(Conn) ->
   ok = epgsql:close(Conn).

select_user(Conn, User, Password) ->
    {ok, _, Res} = epgsql:equery(Conn, "select * from users where username=$1 and password=$2", [ User, Password]),
    Res.
select_users(Conn) ->
    {ok, _, Res} = epgsql:equery(Conn, "select id, username from users", []),
    Res.
list_player(Conn, UserId) ->
    {ok, _, Res} = epgsql:equery(Conn, "select * from players where user_id=$1", [UserId]),
    Res.

save_msg(Conn, UserId, TargetId, Msg) ->
	{ok, Res} = epgsql:equery(Conn, "insert into msgs values ($1, $2, $3) ", [UserId, TargetId, Msg]),
	Res.