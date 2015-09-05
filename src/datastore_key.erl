-module(datastore_key).

%% @doc A key repesent the unique identifier of an object. The Key sheme is
%% inspired by file systems and the google apps engine.
%%
%% Keys are meant unique to the system.  Keys are hierarchical incorporating
%% one or more names spaces.
%%
%% Keys, namespaces and instances are are simple binary.
%%
%% This key are ancestors and children:
%%
%%    <<"/Comedy">>
%%    <<"/Comedy/MontyPython">>.
%%
%% A key namespace is like a path element. It can optionnaly include a
%  type delimied by a `:'. ex:
%%
%%     <<"Song:PhilosopherSong">>, the type is `Song', the value is
%%     `PhilosopherSong'
%%     <<"Music:Song:PhilosopherSong">>, the type is `Music:Song' the value is
%%     `PhilosopherSong'
%%

-export([new/1,
         with_namespaces/1,
         clean/1,
         equal/2,
         less/2,
         to_list/1,
         reverse/1,
         namespaces/1,
         basenamespaces/1,
         type/1,
         name/1,
         path/1,
         parent/1,
         append_child/2,
         instance/2,
         is_ancestor/2,
         is_descendant/2,
         is_toplevel/1,
         namespace_type/1,
         namespace_value/1,
         split_namespace/1]).

-type key() :: binary().
-export_type([key/0]).

%% @doc initialise a key and clean its value
-spec new(binary()) -> key().
new(Key) ->
    clean(Key).

%% @doc construct a key from a namespace list
-spec with_namespaces([binary()]) -> key().
with_namespaces(Ns) when is_list(Ns) ->
    new(datastore_util:join(Ns, <<"/">>)).

%% @doc clean the key, normalize the path
-spec clean(binary()) -> key().
clean(Key0) ->
    Key = datastore_util:to_binary(Key0),
    datastore_util:clean_path(<< "/", Key/binary >>).

%% @doc check if 2 keys are equal
-spec equal(key(), key()) -> true | false.
equal(K1, K2) ->
    K1 =:= K2.

%% @doc check if a key is sorted lower than the other
-spec less(key(), key()) -> true | false.
less(K1, K2) when is_binary(K1), is_binary(K2)->
    less1(to_list(K1), to_list(K2)).

less1([], [_ | _]) ->
    true;
less1(_, []) ->
    false;
less1([A | RestA], [A | RestB]) ->
    less1(RestA, RestB);
less1([A | _RestA], [B | _RestB]) ->
    A < B.

%% @doc return the key as a list of namespaces
%% Ex: <<"/Comedy/MontyPython/Actor:JohnCleese">>
%% [<<"Comedy", <<"MontyPython">>, <<"Actor:JohnCleese">>]
-spec to_list(key()) -> [binary()].
to_list(Key) ->
    [_ | List] = binary:split(Key, <<"/">>, [global]),
    List.

%% @doc return the reverse of this key
%% Ex:<< "/Comedy/MontyPython/Actor:JohnCleese">> return
%% <<"/Actor:JohnCleese/MontyPython/Comedy">>
-spec reverse(key()) -> key().
reverse(Key) -> with_namespaces(lists:reverse(to_list(Key))).

%% @doc return the key as a list of namespaces
%% Ex: <<"/Comedy/MontyPython/Actor:JohnCleese">>
%% [<<"Comedy", <<"MontyPython">>, <<"Actor:JohnCleese">>]
-spec namespaces(key()) -> [binary()].
namespaces(Key) -> to_list(Key).

%% @doc returns the "base" namespace of this key
%% Ex: basenamespaces(<<"/Comedy/MontyPython/Actor:JohnCleese">>)
%% <<"/Actor:JohnCleese/MontyPython/
-spec basenamespaces(key) -> binary().
basenamespaces(Key) ->
    %% we reverse the binary  by bytes order to extract the last item.
    case binary:split(datastore_util:reverse(Key), <<"/">>) of
        [ReversedBase, _] -> datastore_util:reverse(ReversedBase);
        [_] -> Key
    end.

%% @doc return the type of the key (last value of the namespace)
%% ex: type(<<"/Comedy/MontyPython/Actor:JohnCleese">>).
%% <<"Actor">>.
-spec type(key()) -> binary().
type(Key) ->
    namespace_type(basenamespaces(Key)).

%% @doc return the name of the key (last field in the namespaces)
%% ex: name(<<"/Comedy/MontyPython/Actor:JohnCleese">>)
%% JohnCleese
-spec name(key()) -> binary().
name(Key) ->
    namespace_value(basenamespaces(Key)).

%% @doc append a value to the key.
%% exp: instance(<<"/Comedy/MontyPython/Actor">>, <<"JohnCleese">>)
%% <<"/Comedy/MontyPython/Actor:JohnCleese">>"
-spec instance(key(), binary()) -> key().
instance(Key, Value) when is_binary(Value) ->
    new(<< Key/binary, ":", Value/binary >>).

%% @doc return the path of this key (parent + type)
%% ex: path(<<"/Comedy/MontyPython/Actor:JohnCleese">>)
%% <<"/Comedy/MontyPython/Actor">>
path(Key) ->
    BaseName = basenamespaces(Key),
    Type = namespace_type(BaseName),
    case BaseName of
        Key -> << "/", Type/binary >>;
        _ ->
            Len = byte_size(Key) - byte_size(BaseName) - 1,
            Parent = binary:part(Key, 0, Len),
            new(<< Parent/binary, "/", Type/binary >>)
    end.



%% @doc extract the parent key of the key
%% ex: parent(<<"/Comedy/MontyPython/Actor:JohnCleese">>)
%% <<"/Comedy/MontyPython">>
-spec parent(key()) -> key().
parent(Key) ->
    case basenamespaces(Key) of
        Key -> <<"/">>;
        BaseName ->
            Len = byte_size(Key) - byte_size(BaseName) - 1,
            new(binary:part(Key, 0, Len))
    end.

%% @doc append a child to a key
%% ex: append_child(<<"/Comedy/MontyPython">>, <<"Actor:JohnCleese">>)
%% <<"/Comedy/MontyPython/Actor:JohnCleese">>
-spec append_child(key(), binary()) -> key().
append_child(Parent, Child) ->
    new(<< Parent/binary, "/", Child/binary >>).

%% @doc check if the key is a prefix of other
%% ex: is_ancestor(<<"/Comedy">>, <<"/Comedy/MontyPython">>)
%% true
-spec is_ancestor(key(), key()) -> true | false.
is_ancestor(Key, Key) -> false;
is_ancestor(Key, Other) ->
    Sz = byte_size(Key),
    case Other of
        << Key:Sz/binary, _/binary >> -> true;
        _ -> false
    end.

%% @doc check if the key contains another key
%% ex: is_decendant(<<"/Comedy/MontyPython">>, <<"/Comedy">>)
%% true
-spec is_descendant(key(), key()) -> true | false.
is_descendant(Key, Other) ->
    is_ancestor(Other, Key).

%% check if a key has only one namespace
-spec is_toplevel(key()) -> true | false.
is_toplevel(Key) ->
    length(to_list(Key)) == 1.

%% @doc return the first component of a namespace. `foo' in `foo:bar'
-spec namespace_type(binary()) -> binary().
namespace_type(Namespace) ->
    {Type, _} = split_namespace(Namespace),
    Type.

%% @doc return the last component of a namespace. `bar' in `foo:bar'
-spec namespace_value(binary()) -> binary().
namespace_value(Namespace) ->
    {_, Value} = split_namespace(Namespace),
    Value.

%% split the namespace in `{type, value}`' tuple.
-spec split_namespace(binary()) -> {binary(), binary()}.
split_namespace(Namespace) ->
    Reversed = datastore_util:reverse(Namespace),
    case binary:split(Reversed, <<":">>) of
        [Reversed] -> {<<>>, Namespace};
        [RNamespaceValue, _] ->
            Sz = byte_size(Namespace),
            VSz = byte_size(RNamespaceValue),
            TSz = Sz - VSz -1,
            << Type:TSz/binary, ":", Value/binary>> = Namespace,
            {Type, Value}
    end.



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


new_test() ->
    ?assertEqual(<<"/test">>, new("test")),
    ?assertEqual(<<"/test">>, new(<<"test">>)),
    ?assertEqual(<<"/test">>, new(<<"/test">>)),
    ?assertEqual(<<"/a/b/c">>, new(<<"/a/b/c">>)),
    ?assertEqual(<<"/a/b/c">>, new(<<"a/b/c">>)),
    ?assertEqual(<<"/c">>, new(<<"a/../c">>)),
    ?assertEqual(<<"/c">>, new(<<"/a/../c">>)).

with_namespaces_test() ->
    ?assertEqual(<<"/a/b/c">>, with_namespaces([<<"a">>, <<"b">>, <<"c">>])),
    ?assertEqual(<<"/a/b/c">>, with_namespaces([<<"/a">>, <<"b">>, <<"c">>])),
    ?assertEqual(<<"/a/b/c/1">>, with_namespaces([<<"/a">>, <<"b">>, <<"c">>,
                                                 1])).

compare_test() ->
    ?assertMatch(true, equal(<<"/A">>, <<"/A">>)),
    ?assertMatch(false, equal(<<"/A">>, <<"/B">>)),
    ?assertMatch(true, equal(<<"/a/b/c">>, <<"/a/b/c">>)),
    ?assertMatch(true, equal(<<"/a/b/c:d">>, <<"/a/b/c:d">>)),
    ?assertMatch(false, equal(<<"/a/b/c">>, <<"/a/b/d">>)),
    ?assertMatch(false, equal(<<"/a/b/c">>, <<"/a/b/c/d">>)),
    ?assertMatch(false, equal(<<"/a/b/c:d">>, <<"/a/b/c:e">>)),

    ?assertMatch(false, less(<<"/a">>, <<"/a">>)),
    ?assertMatch(true, less(<<"/a">>, <<"/b">>)),
    ?assertMatch(true, less(<<"/a">>, <<"/aa">>)),
    ?assertMatch(false, less(<<"/aa">>, <<"/a">>)),
    ?assertMatch(true, less(<<"/a">>, <<"/a/b">>)),
    ?assertMatch(false, less(<<"/a/b">>, <<"/a">>)),
    ?assertMatch(true, less(<<"/a/b/c">>, <<"/a/b/d">>)),
    ?assertMatch(false, less(<<"/a/b/c">>, <<"/a/b/c">>)),
    ?assertMatch(true, less(<<"/a/b/c">>, <<"/a/c/c">>)),
    ?assertMatch(true, less(<<"/a/b/c">>,<<"/a/b/c/d">>)),
    ?assertMatch(false, less(<<"/a/c/c">>, <<"/a/b/c">>)),
    ?assertMatch(false, less(<<"/a/c/c">>, <<"/a/b/c/d">>)).

namespaces_test() ->
    Namespaces = [<<"Comedy">>, <<"MontyPython">>, <<"Actor:JohnCleese">>],
    Key = with_namespaces(Namespaces),
    %% test namespaces transformation to key
    ?assertMatch(<<"/Comedy/MontyPython/Actor:JohnCleese">>, Key),
    ?assertMatch(Namespaces, to_list(Key)),
    ?assertMatch(Namespaces, namespaces(Key)),
    ?assertMatch(<<"/Actor:JohnCleese/MontyPython/Comedy">>, reverse(Key)),
    %% test namespaces extraction
    ?assertMatch(<<"Actor:JohnCleese">>, basenamespaces(Key)),
    ?assertMatch({<<"Song">>, <<"PhilosopherSong">>},
                 split_namespace(<<"Song:PhilosopherSong">>)),
    ?assertMatch({<<"Music:Song">>, <<"PhilosopherSong">>},
                 split_namespace(<<"Music:Song:PhilosopherSong">>)),
    ?assertMatch(<<"Song">>, namespace_type(<<"Song:PhilosopherSong">>)),
    ?assertMatch(<<"Music:Song">>,
                 namespace_type(<<"Music:Song:PhilosopherSong">>)),
    ?assertMatch(<<"PhilosopherSong">>,
                 namespace_value(<<"Song:PhilosopherSong">>)),
    ?assertMatch(<<"PhilosopherSong">>,
                 namespace_value(<<"Music:Song:PhilosopherSong">>)),

    ?assertMatch(<<"Actor">>, type(Key)),
    ?assertMatch(<<"JohnCleese">>, name(Key)),
    ?assertMatch(Key, instance(<<"/Comedy/MontyPython/Actor">>,
                               <<"JohnCleese">>)),
    ?assertMatch(<<"/Comedy/MontyPython/Actor">>,
                 path(<<"/Comedy/MontyPython/Actor:JohnCleese">>)),
    ?assertMatch(<<"/Comedy/MontyPython">>, parent(Key)),
    ?assertMatch(Key, append_child(<<"/Comedy/MontyPython">>, <<"Actor:JohnCleese">>)),
    ?assertMatch(true, is_ancestor(<<"/Comedy">>, Key)),
    ?assertMatch(true, is_descendant(<<"/Comedy/MontyPython">>,
                                     <<"/Comedy">>)),
    ?assertMatch(true, is_toplevel(<<"/comedy">>)),
    ?assertMatch(false, is_toplevel(Key)).


-endif.
