-module(game_db).
-export([open/0, 
         close/1,
         select_user/3,
         select_users/1,
         list_player/2]).

open() ->
    {Addr, User, Password, Dbname} = init(),
    {ok, Conn}=epgsql:connect(Addr, User, Password,
                              [
                               { database, Dbname,
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

init() ->
  {ok, F} = file:read_file("db.dat"),
  Data = re:split(F, "\n"),
  D =lists:foldl(fun(E, Dict) -> 
      [K, V] = re:split(E, ":"),
      D = maps:put(binary:bin_to_list(K), binary:bin_to_list(V), Dict), D 
    end, #{}, Data),
  io:format("Data~p~n", [D]),
  {
    maps:get("dbaddr", D), 
    maps:get("username", D),
    maps:get("password", D),
    maps:get("database", D)
  }.