%% -------------------------------------------------------------------
%% sws_sub.erl - SWS supervisor module
%% 
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2010,2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS supervisor module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010,2011 Martijn Rijkeboer
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
-module(sws_sup).
-author('Martijn Rijkeboer <martijn@bunix.org>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).


%% -------------------------------------------------------------------
%% @spec start_link() ->
%%        ServerRet
%% @doc API for starting the supervisor.o
%% @end
%% -------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% @spec upgrade() ->
%%        ok
%% @doc Add processes if necessary.
%% @end
%% -------------------------------------------------------------------
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list(
    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id),
        ok
    end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.


%% -------------------------------------------------------------------
%% @spec init([]) ->
%%        SupervisorTree
%% @doc Supervisor callback.
%% @end
%% -------------------------------------------------------------------
init([]) ->
  Ip = sws_config:ip(),
  Port = sws_config:port(),
  DispatchConfPath = sws_config:dispatch_conf_path(),
  LogDir = sws_config:log_dir_path(),

  {ok, Dispatch} = file:consult(DispatchConfPath),

  WebConfig = [
    {ip, Ip},
    {port, Port},
    {log_dir, LogDir},
    {dispatch, Dispatch}
  ],

  Web = {
    webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, dynamic
  },

  {ok, { {one_for_one, 10, 10}, [Web]} }.

