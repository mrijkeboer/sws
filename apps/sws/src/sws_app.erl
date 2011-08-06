%% -------------------------------------------------------------------
%% sws_app.erl - SWS application module
%% 
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS application module
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
-module(sws_app).
-author('Martijn Rijkeboer <martijn@bunix.org>').

-behaviour(application).

%% API
-export([
    start/2,
    stop/1
  ]).


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec start(_Type, _StartArgs) ->
%%        ServerRet
%% @doc Application start callback for SWS.
%% @end
%% -------------------------------------------------------------------
start(_Type, _StartArgs) ->
    sws_sup:start_link().


%% -------------------------------------------------------------------
%% @spec stop(_State) ->
%%        ServerRet
%% @doc Application stop callback for SWS.
%% @end
%% -------------------------------------------------------------------
stop(_State) ->
    ok.
