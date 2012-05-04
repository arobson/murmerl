%%% @author Alex Robson
%%% @copyright Alex Robson, 2012
%%% @doc
%%%
%%%	
%%%
%%% @end
%%% @license MIT
%%% Created May 3, 2012 by Alex Robson

-module(murmerl).
-version(0.1).
-on_load(init/0).
-export([
	hash_x86_32/1,
	hash_x86_128/1,
	hash_x64_128/1]).

init() ->
	random:seed(now()),
	case code:priv_dir(murmerl) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "murmerl_nif"]);
                _ ->
                    SoName = filename:join("../priv", "murmerl_nif")
            end;
        Dir ->
            SoName = filename:join(Dir, "murmerl_nif")
    end,
    case erlang:load_nif(SoName, 0) of
    	ok -> io:format("murmerl_nif loaded~n");
    	{error, {Reason, Text}} ->
    		io:format("Failed to load murmerl_nif ~p :: ~p ~n", [Reason, Text])
    end.

hash_32(Key) when is_binary(Key) ->
	hash_x86_32_impl(Key, random:uniform(1000000));

hash_32(Key) when is_list(Key) ->
	hash_32(list_to_binary(Key));

hash_32(Key) when is_atom(Key) ->
	hash_32(term_to_binary(Key)).


hash_128(Key) when is_binary(Key) ->
	List = hash_x86_128_impl(Key, random:uniform(1000000)),
	merge_int_list_to_bignum(List);

hash_128(Key) when is_list(Key) ->
	hash_128(list_to_binary(Key));

hash_128(Key) when is_atom(Key) ->
	hash_128(term_to_binary(Key)).


hash_x64_128(Key) when is_binary(Key) ->
	List = hash_x64_128_impl(Key, random:uniform(1000000)),
	merge_int_list_to_bignum(List);

hash_x64_128(Key) when is_list(Key) ->
	hash_x64_128(list_to_binary(Key));

hash_x64_128(Key) when is_atom(Key) ->
	hash_x64_128(term_to_binary(Key)).

hash_x86_32_impl(_, _) ->
	exit(nif_lib_not_loaded).

hash_x86_128_impl(_, _) ->
	exit(nif_lib_not_loaded).

hash_x64_128_impl(_, _) ->
	exit(nif_lib_not_loaded).

merge_int_list_to_bignum(List) ->
	binary_to_term(
		binary:list_to_bin(
			lists:flatten(
				lists:map(
					fun(X) -> 
						binary:bin_to_list(
							term_to_binary(X)
						) 
					end, 
				List)
			)
		)
	).