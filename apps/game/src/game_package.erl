-module(game_package).

-export([pack/2, unpack/1, unpack_raw/2]).

helper({K1, K2}) ->
    case is_binary(K2) of
	true -> {binary_to_list(K1), binary_to_list(K2)};
	false -> {binary_to_list(K1), K2}
    end.

 	    
change(M) ->
    DataList = maps:to_list(M),
    ChangeList = lists:map(fun helper/1, DataList),
    maps:from_list(ChangeList).

unpack_raw(Method, Data) ->
    M = list_to_atom(binary_to_list(Method)),
    Json = jiffy:decode(Data, [return_maps]),
    {M, change(Json)}.    

%% pack & unpack
pack(Method, Data) ->
  MethodLen = length(binary_to_list(Method)),
  <<MethodLen:8, Method/binary, Data/binary>>.

unpack(RawData) ->
  <<MethodLen:8, Bin/binary>> = RawData,
  <<Method:MethodLen/binary, Data/binary>> = Bin,
  {Method, Data}.