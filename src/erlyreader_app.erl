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

-module(erlyreader_app).
-author('frederick.grim@gmail.com').
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", reader_handler, []},
            {"/auth/:provider/:action", cowboy_social, [{<<"google">>, get_social_params() ++ [
                {callback_uri, <<"/oauth2callback">>},
                {scope, << "https://www.googleapis.com/auth/userinfo.email ", "https://www.googleapis.com/auth/userinfo.profile" >>},
                {authorize_uri, <<"https://accounts.google.com/o/oauth2/auth">>},
                {token_uri, <<"https://accounts.google.com/o/oauth2/token">>}]}]},
            {"/oauth2callback", erlyreader_session_handler, []}
	    ]}
    ]),
	{ok, _} = cowboy:start_http(http, 100, [{port, application:get_env(erlyreader, webport, 8001)}],
                                           [{env, [{dispatch, Dispatch}]}]),
    erlyetl_sup:start_link().

stop(_) -> ok.

%% Private

get_social_params() ->
    ClientSecret = application:get_env(erlyreader, google_client_secret, <<"...">>), 
    ClientID = application:get_env(erlyreader, google_client_id, <<"...">>), 
    [{client_id, ClientID}, {client_secret, ClientSecret}].

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
