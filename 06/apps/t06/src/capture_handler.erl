-module(capture_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Custom callbacks.
-export([create_paste/2]).
-export([paste_html/2]).
-export([paste_text/2]).

init(_Transport, _Req, []) ->
    % For the random number generator:
    {X, Y, Z} = now(),
    random:seed(X, Y, Z),
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"plain">>, []}, paste_text},
        {{<<"text">>, <<"html">>, []}, paste_html}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_paste}],
        Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(paste_id, Req) of
        {undefined, Req2} ->
            {true, Req2, index};
        {PasteID, Req2} ->
            case valid_path(PasteID) and file_exists(PasteID) of
                true -> {true, Req2, PasteID};
                false -> {false, Req2, PasteID}
            end
    end.

create_paste(Req, State) ->
    lager:debug("req: ~p~n", [Req]),
    PasteID = new_paste_id(),
    Res1 = cowboy_req:body(Req),
    {ok, Paste, Req3} = Res1,
    process_data(Paste),
    ok = file:write_file(full_path(PasteID), Paste),
    case cowboy_req:method(Req3) of
        {<<"POST">>, Req4} ->
            {{true, <<$/, PasteID/binary>>}, Req4, State};
        {_, Req4} ->
            {true, Req4, State}
    end.

paste_html(Req, index) ->
    {read_file("index.html"), Req, index};
paste_html(Req, Paste) ->
    {Style, Req2} = cowboy_req:qs_val(<<"lang">>, Req, plain),
    {format_html(Paste, Style), Req2, Paste}.

paste_text(Req, index) ->
    {read_file("index.txt"), Req, index};
paste_text(Req, Paste) ->
    {Style, Req2} = cowboy_req:qs_val(<<"lang">>, Req, plain),
    {format_text(Paste, Style), Req2, Paste}.

% Private

read_file(Name) ->
    {ok, Binary} = file:read_file(full_path(Name)),
    Binary.

full_path(Name) ->
    Dir = code:priv_dir(t06),
    filename:join([Dir, Name]).

file_exists(Name) ->
    case file:read_file_info(full_path(Name)) of
        {ok, _Info} -> true;
        {error, _Reason} -> false
    end.

valid_path(<<>>) -> true;
valid_path(<<$., _T/binary>>) -> false;
valid_path(<<$/, _T/binary>>) -> false;
valid_path(<<_Char, T/binary>>) -> valid_path(T).

new_paste_id() ->
    Initial = random:uniform(62) - 1,
    new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    << <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_paste_id(Bin, Rem) ->
    Next = random:uniform(62) - 1,
    new_paste_id(<<Bin/binary, Next>>, Rem - 1).

format_html(Paste, plain) ->
    Text = escape_html_chars(read_file(Paste)),
    <<"<!DOCTYPE html><html>",
    "<head><title>paste</title></head>",
    "<body><pre><code>", Text/binary, "</code></pre></body></html>\n">>;
format_html(Paste, Lang) ->
    highlight(full_path(Paste), Lang, "html").

format_text(Paste, plain) ->
    read_file(Paste);
format_text(Paste, Lang) ->
    highlight(full_path(Paste), Lang, "ansi").

highlight(Path, Lang, Type) ->
    Path1 = binary_to_list(Path),
    Lang1 = binary_to_list(Lang),
    os:cmd(["highlight --syntax=", Lang1,
        " --doc-title=paste ",
        " --out-format=", Type,
        " --include-style ", Path1]).

% Escape some HTML characters that might make a fuss
escape_html_chars(Bin) ->
    << <<(escape_html_char(B))/binary>> || <<B>> <= Bin >>.

escape_html_char($<) -> <<"&lt;">>;
escape_html_char($>) -> <<"&gt;">>;
escape_html_char($&) -> <<"&amp;">>;
escape_html_char(C) -> <<C>>.

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

