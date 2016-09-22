-module(capture_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% Custom callbacks.
-export([create_paste/2]).

init(_Transport, _Req, []) ->
    % For the random number generator:
    {X, Y, Z} = now(),
    random:seed(X, Y, Z),
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_paste}],
        Req, State}.

create_paste(Req, State) ->
    lager:debug("req: ~p~n", [Req]),
    Res1 = cowboy_req:body(Req),
    {ok, Paste, Req3} = Res1,
    process_data(Paste),
    case cowboy_req:method(Req3) of
        {<<"POST">>, Req4} ->
            {{true, <<"/">>}, Req4, State};
        {_, Req4} ->
            {true, Req4, State}
    end.

% Private

process_data(Data) ->
    {ok, E1} = exml:parse(Data),
    lager:debug("el: ~p~n", [E1]),
    Vals = get_base_children(E1),
    case is_valid(Vals) of
        true ->
            store_item(Vals);
        false ->
            {error, invalid_item}
    end.

is_valid(Vals) ->
    case find_gtin(Vals) of
        undefined ->
            false;
        _ ->
            case find_name(Vals) of
                undefined ->
                    false;
                _ ->
                    true
            end
    end.

get_base_children(E) ->
    Path = build_xml_path(),
    E2 = exml_query:path(E, Path),
    exml_query:paths(E2, [{element, <<"value">>}]).

build_xml_path() ->
    Items = [<<"S:Body">>,
            <<"ns2:GetItemByGTINResponse">>,
            <<"ns2:GS46Item">>,
            <<"DataRecord">>,
            <<"record">>,
            <<"BaseAttributeValues">>],
    [{element, X} || X <- Items].

store_item(Vals) ->
    Gtin = find_gtin(Vals),
    Name = find_name(Vals),
    Desc = find_desc(Vals),
    Company = find_company(Vals),
    item_writer:store(Gtin, Name, Desc, Company).

find_gtin(Vals) ->
    find_item(<<"PROD_COVER_GTIN">>, Vals).

find_name(Vals) ->
    find_item(<<"PROD_NAME">>, Vals).

find_desc(Vals) ->
    find_item(<<"PROD_DESC">>, Vals).

find_company(Vals) ->
    find_item(<<"BRAND_OWNER_NAME">>, Vals).

find_item(Key, L) ->
    L2 = [Item || Item <- L, is_valid_item(Item, Key)],
    case L2 of
        [] ->
            undefined;
        [Val | _] ->
            exml_query:attr(Val, <<"value">>)
    end.

is_valid_item(Item, Key) ->
    exml_query:attr(Item, <<"baseAttrId">>) =:= Key.

