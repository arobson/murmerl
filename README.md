## Murmerl
An Erlang NIF wrapper for the native Murmurhash3 library. Why? Because murmurhash3 is crazy fast and relatively low collision. I am working on a consistent hashing implementation and this should improve the throughput.

**NOTE** This is the result of an few hours of hacking. It does appear to be stable now.

## Build

	make

or

	rebar compile

## API

x86 is the default implementation, if you want the x64 call, you get the more verbose function form.

hash_32( Key ) -> integer
	Types:

		**Key = binary | atom | list**

hash_128( Key ) -> integer
	Types:

		**Key = binary | atom | list**


hash_x64_128( Key ) -> integer
	Types:

		**Key = binary | atom | list**

## Playtime

A couple of fun things you can do to kick the tires in Erlang shell.

	make start

Once it comes up, you still have to load the module with:

	code:load_file(murmerl).

After that, you're good to go. Since I'm doing this largely for the purpose of building up virtual nodes to create an evenly distributed key-space, the thing I'm most interested in is how well this does and how long it takes.

	%% X == the number of keys to produce
	%% Y == the number to divide the result by
	%% returns { microseconds, ListOfKeys }
	F1 = fun( X, Y ) -> timer:tc( fun() -> [ murmerl:hash_128(integer_to_list(Z)) rem Y || Z <- lists:seq(1,X) ] end ) end.

	%% {_,_} == the result of F1
	%% returns dict where K is each key and V is the number of times
	%% the key appeared in the list.
	%% this function will show you the 'sample'
	F2 = fun({_,L}) -> lists:foldl( fun(X,A) -> dict:update_counter( X, 1, A) end, dict:new(), L) end.

	%% this means you can call F2( F1( 1000, 100 ) ).

	%% This will allow you to calculate the standard deviation of the list.
	%% Fun times.
	StdDev = fun({_,L}) -> M = lists:sum(L)/length(L), Div = lists:foldl( fun(X,A) -> A + math:pow(X-M,2) end, 0, L), math:sqrt(Div/length(L)) end.

	%% to use it call: StdDev( F1( 100, 10) ).