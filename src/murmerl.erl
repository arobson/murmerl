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
    	ok -> io:format("Nif loaded, broseppe~n");
    	{error, {Reason, Text}} ->
    		io:format(":@ ~p :: ~p ~n", [Reason, Text])
    end.

hash_x86_32(Key) when is_binary(Key) ->
	hash_x86_32_impl(Key, random:uniform(1000000));

hash_x86_32(Key) when is_list(Key) ->
	hash_x86_32(list_to_binary(Key));

hash_x86_32(Key) when is_atom(Key) ->
	hash_x86_32(term_to_binary(Key)).


hash_x86_128(Key) when is_binary(Key) ->
	hash_x86_128_impl(Key, random:uniform(1000000));

hash_x86_128(Key) when is_list(Key) ->
	hash_x86_128(list_to_binary(Key));

hash_x86_128(Key) when is_atom(Key) ->
	hash_x86_128(term_to_binary(Key)).


hash_x64_128(Key) when is_binary(Key) ->
	hash_x64_128_impl(Key, random:uniform(1000000));

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