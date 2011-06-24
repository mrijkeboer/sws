%% -------------------------------------------------------------------
%% sws_config.erl - SWS application configuration module
%% 
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS application configuration module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2011 Martijn Rijkeboer
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
-module(sws_config).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		ip/0,
		port/0,
		dispatch_conf_path/0,
		log_dir_path/0,
		www_root_path/0,
		page_expire_time/0,
		file_expire_time/0,
		lib_expire_time/0,
		misc_expire_time/0
	]).


%% -------------------------------------------------------------------
%% @spec ip() ->
%%				Value
%% @doc Returns the IP address to bind to.
%% @end
%% -------------------------------------------------------------------
ip() ->
	get_value(ip, "127.0.0.1").


%% -------------------------------------------------------------------
%% @spec port() ->
%%				Value
%% @doc Returns the port to listen on.
%% @end
%% -------------------------------------------------------------------
port() ->
	get_value(port, 8000).


%% -------------------------------------------------------------------
%% @spec dispatch_conf_path() ->
%%				Value
%% @doc Returns the path to the dispatch.conf file.
%% @end
%% -------------------------------------------------------------------
dispatch_conf_path() ->
	get_value(dispatch_conf_path, "priv/dispatch.conf").


%% -------------------------------------------------------------------
%% @spec log_dir_path() ->
%%				Value
%% @doc Returns the path to the log directory.
%% @end
%% -------------------------------------------------------------------
log_dir_path() ->
	get_value(log_dir_path, "priv/log").


%% -------------------------------------------------------------------
%% @spec www_root_path() ->
%%				Value
%% @doc Returns the path to the www root directory.
%% @end
%% -------------------------------------------------------------------
www_root_path() ->
	get_value(www_root_path, "priv").


%% -------------------------------------------------------------------
%% @spec page_expire_time() ->
%%				Value
%% @doc Returns the expire time for pages.
%% @end
%% -------------------------------------------------------------------
page_expire_time() ->
	get_value(file_expire_time, 0).


%% -------------------------------------------------------------------
%% @spec file_expire_time() ->
%%				Value
%% @doc Returns the expire time for files.
%% @end
%% -------------------------------------------------------------------
file_expire_time() ->
	get_value(file_expire_time, 0).


%% -------------------------------------------------------------------
%% @spec lib_expire_time() ->
%%				Value
%% @doc Returns the expire time for library files (css, js, ...).
%% @end
%% -------------------------------------------------------------------
lib_expire_time() ->
	get_value(lib_expire_time, 0).


%% -------------------------------------------------------------------
%% @spec misc_expire_time() ->
%%				Value
%% @doc Returns the expire time for miscellaneous files (robots.txt).
%% @end
%% -------------------------------------------------------------------
misc_expire_time() ->
	get_value(misc_expire_time, 0).


%% -------------------------------------------------------------------
%% @spec get_value(Par, DefaultValue) ->
%%				Value |
%%				DefaultValue
%% @doc Returns the value of the configuration parameter Par or
%% DefaultValue if the parameter doesn't exist.
%% @end
%% -------------------------------------------------------------------
get_value(Par, DefaultValue) ->
	case application:get_env(sws, Par) of
		{ok, Value} ->
			Value;
		_ ->
			DefaultValue
	end.


