%% Copyright (c) 2011-2013, Frederick Grim <frederick.grim@gmail.com>

%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.

%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 

-module(erlyreader_session_handler).
-author('frederick.grim@gmail.com').

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req} = cowboy_req:method(Req),
    {ok, ReqReply} = handle_session_method(list_to_atom(binary_to_list(Method)), Req),
	{ok, ReqReply, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Private methods

handle_session_method('GET', Req) ->
    {Path, Req} = cowboy_req:path(Req) 
    handle_path_selector(list_to_atom(binary_to_list(Path)), Req).

handle_path_selector('/oauth2callback', Req) ->
    handle_oauth2code(cowboy_req:qs_val(<<"code">>, Req), Req).

handle_oauth2code({undefined, _}, Req) ->
    cowboy_req:reply(403, [], <<"Forbidden">>, Req);

handle_oauth2code({Code, _}, Req) when is_binary(Code) ->

    try cowboy_social_google:user_profile(Code, []) of
        {ok, AttrList} ->
            case proplists:get_value(email, AttrList) of
                undefined ->
                    handle_oauth2code({undefined, Req}, Req);
                Email ->
                    {ok, SessionID} = session_utils:create_session(Email),
                    {Cookie, CookieReq} = cowboy_req:cookie(list_to_binary(SessionID), Req),
                    cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], <<"Redirected">>, CookieReq)
            end
    catch
        Err ->
            error_logger:error_msg(),
            handle_oauth2code({undefined, Req}, Req)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
