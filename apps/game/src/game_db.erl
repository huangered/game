-module(game_db).
-export([open/0, close/1, select_user/3]).

open() ->
    {ok, Conn}=epgsql:connect("localhost", "huangered", "1234",
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
