%% -------------------------------------------------------------------
%% sws.erl - SWS startup module
%% 
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS startup module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010 Martijn P. Rijkeboer
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%%
%% -------------------------------------------------------------------
-module(sws).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
    start/0,
    start_link/0,
    stop/0,
    stop/1
  ]).

%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec ensure_started(App) ->
%%        ok
%% @doc Make shure  the requested app is started.
%% @end
%% -------------------------------------------------------------------
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.


%% -------------------------------------------------------------------
%% @spec start_link() ->
%%        {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
%% @end
%% -------------------------------------------------------------------
start_link() ->
  ensure_started(crypto),
  ensure_started(mochiweb),
  application:set_env(
    webmachine,
    webmachine_logger_module,
    webmachine_logger
  ),
  ensure_started(webmachine),
  sws_sup:start_link().


%% -------------------------------------------------------------------
%% @spec start() ->
%%        ok
%% @doc Start the sws server.
%% @end
%% -------------------------------------------------------------------
start() ->
  ensure_started(crypto),
  ensure_started(mochiweb),
  application:set_env(
    webmachine,
    webmachine_logger_module,
    webmachine_logger
  ),
  ensure_started(webmachine),
  application:start(sws).


%% -------------------------------------------------------------------
%% @spec stop() ->
%%        ok
%% @doc Stop the sws server.
%% @end
%% -------------------------------------------------------------------
stop() ->
  Res = application:stop(sws),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(crypto),
  Res.


%% -------------------------------------------------------------------
%% @spec stop([Node]) ->
%%        void
%% @doc Stop the sws server on the specified node.
%% @end
%% -------------------------------------------------------------------
stop([Node]) ->
  case net_adm:ping(Node) of
    pong -> rpc:cast(Node, init, stop, []);
    _ -> io:format("No node named: ~s~n", [Node])
  end,
  init:stop().
