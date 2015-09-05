-module(datastore_util).

-export([to_binary/1,
         join/2,
         reverse/1,
         clean_path/1]).

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).


join([], _Separator) ->
    <<>>;
join([S], _Separator) ->
    S;
join(L, Separator) when is_binary(Separator) ->
    iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
    Acc;
join([S | Rest], Separator, []) ->
    join(Rest, Separator, [to_binary(S)]);
join([S | Rest], Separator, Acc) ->
    join(Rest, Separator, [to_binary(S), Separator | Acc]).

reverse(Bin) ->
    Sz = bit_size(Bin),
    << X:Sz/integer-little >> = Bin,
    << X:Sz/integer-big >>.


clean_path(Path) ->
    Parts = clean_path1(binary:split(Path, <<"/">>, [global]), []),
    << "/", (join(Parts, <<"/">>))/binary >>.

clean_path1([], Acc) ->
    lists:reverse(Acc);
clean_path1([<<>>|Rest], Acc) ->
    clean_path1(Rest, Acc);
clean_path1([<<"..">>|Rest], Acc) ->
    Acc1 = case Acc of
               [] -> Acc;
               [T|_] when T =:= <<"..">> -> [<<"..">>|Acc];
               [_|R] -> R
           end,
    clean_path1(Rest, Acc1);
clean_path1([<<".">>|Rest], Acc) ->
    clean_path1(Rest, Acc);
clean_path1([Path|Rest], Acc) ->
    clean_path1(Rest, [Path|Acc]).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_binary_test() ->
    ?assertMatch(<<"a">>, to_binary(<<"a">>)),
    ?assertMatch(<<"a">>, to_binary("a")),
    ?assertMatch(<<"a">>, to_binary(a)),
    ?assertMatch(<<"1">>, to_binary(1)).

join_test() ->
    ?assertMatch(<<"a/b">>, join([<<"a">>, <<"b">>], <<"/">>)),
    ?assertMatch(<<"a/b">>, join([<<"a">>, "b"], <<"/">>)),
    ?assertMatch(<<"a/b/c/1">>, join([<<"a">>, "b", c, 1], <<"/">>)).


reverse_test() ->
    ?assertMatch(<<"c/b/a">>, reverse(<<"a/b/c">>)),
    ?assertMatch(<<"c/b/a/">>, reverse(<<"/a/b/c">>)),
    ?assertMatch(<<"d:c/b/a/">>, reverse(<<"/a/b/c:d">>)).


clean_path_test() ->
    ?assertMatch(<<"/">>, clean_path(<<"/..">>)),
    ?assertMatch(<<"/a">>, clean_path(<<"/../a">>)),
    ?assertMatch(<<"/a/b">>, clean_path(<<"/a////b">>)),
    ?assertMatch(<<"/a/c">>, clean_path(<<"/a/b/../c">>)).

-endif.

