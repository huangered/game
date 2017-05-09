-module(game_package).

-export([pack/2, unpack/1]).


pack(Method, Data) ->
	MethodLen = length(binary_to_list(Method)),
	<<MethodLen: 8, Method/binary, Data/binary>>.

unpack(RawData) ->
	<<MethodLen: 8, Bin/binary>> = RawData,
	<<Method:MethodLen/binary, Data/binary>> = Bin,
	{Method, Data}.
