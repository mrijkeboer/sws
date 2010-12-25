%% -------------------------------------------------------------------
%% app_util.erl - OTP application utilities module
%% 
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS utilities module
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
-module(app_util).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		get_env/2,
		get_env/3,
		set_env/3
	]).


%% -------------------------------------------------------------------
%% @spec get_env(Application, Par) ->
%%				Value |
%%				undefined
%% @doc Returns the value of the configuration parameter Par for the
%% Application.
%% @end
%% -------------------------------------------------------------------
get_env(Application, Par) ->
	get_env(Application, Par, undefined).


%% -------------------------------------------------------------------
%% @spec get_env(Application, Par, DefaultValue) ->
%%				Value |
%%				DefaultValue
%% @doc Returns the value of the configuration parameter Par for the
%% Application or DefaultValue if the parameter doesn't exist.
%% @end
%% -------------------------------------------------------------------
get_env(Application, Par, DefaultValue) ->
	case application:get_env(Application, Par) of
		{ok, Value} ->
			Value;
		_ ->
			DefaultValue
	end.


%% -------------------------------------------------------------------
%% @spec set_env(Application, Par, Value) ->
%%				ok
%% @doc Sets the value of the configuration parameter Par for the
%% Application.
%% @end
%% -------------------------------------------------------------------
set_env(Application, Par, Value) ->
	application:set_env(Application, Par, Value).

