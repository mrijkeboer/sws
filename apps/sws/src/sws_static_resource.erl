%% -------------------------------------------------------------------
%% sws_static_resource.erl - SWS static files resource module
%% 
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS static files resource module
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
-module(sws_static_resource).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		init/1,
		content_types_provided/2,
		resource_exists/2,
		to_binary/2,
		last_modified/2,
		expires/2
	]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec init(State) ->
%%				{ok, State}
%% @doc Initialize resource.
%% @end
%% -------------------------------------------------------------------
init(State) -> 
	case proplists:get_value(type, State) of
		undefined ->
			{ok, State ++ [{type, file}]};
		_ ->
			{ok, State}
	end.


%% -------------------------------------------------------------------
%% @spec content_types_provided(ReqData, State) ->
%%				{[{MediaType, Handler}]}, ReqData, State}
%% @doc Set the content types that are provided by this resource.
%% @end
%% -------------------------------------------------------------------
content_types_provided(ReqData, State) ->
	case proplists:get_value(mime, State) of
		undefined ->
			Uri = wrq:path(ReqData),
			Type = proplists:get_value(type, State),
			FsPath = sws_util:static_fs_path(Uri, Type),
			Mime = webmachine_util:guess_mime(FsPath),
			State1 = State ++ [{fs_path, FsPath}, {mime, Mime}],
			{[{Mime, to_binary}], ReqData, State1};
		Mime ->
			{[{Mime, to_binary}], ReqData, State}
	end.


%% -------------------------------------------------------------------
%% @spec resource_exists(ReqData, State) ->
%%				{true, ReqData, State} |
%%				{false, ReqData, State}
%% @doc Check if the requested resource exists (file).
%% @end
%% -------------------------------------------------------------------
resource_exists(ReqData, State) ->
	FsPath = proplists:get_value(fs_path, State),
	case sws_util:file_readable(FsPath) of
		{true, FileInfo} ->
			State1 = State ++ [{file_info, FileInfo}],
			{true, ReqData, State1};
		_ ->
			{false, ReqData, State}
	end.


%% -------------------------------------------------------------------
%% @spec to_binary(ReqData, State) ->
%%				{Content, ReqData, State}
%% @doc Return the content of the resource as binary data.
%% @end
%% -------------------------------------------------------------------
to_binary(ReqData, State) ->
	FsPath = proplists:get_value(fs_path, State),
	{ok, Content} = file:read_file(FsPath),
	{Content, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec last_modified(ReqData, State) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the last modified time of the resource (file).
%% @end
%% -------------------------------------------------------------------
last_modified(ReqData, State) ->
	FileInfo = proplists:get_value(file_info, State),
	{FileInfo#file_info.mtime, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec expires(ReqData, State) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the expire date of the resource (file).
%% @end
%% -------------------------------------------------------------------
expires(ReqData, State) ->
	UtcNow = calendar:universal_time(),
	case proplists:get_value(type, State) of
		file ->
			Expires = sws_util:add_seconds_to(UtcNow, 24 * 60 * 60);
		lib ->
			Expires = sws_util:add_seconds_to(UtcNow, 60 * 60);
		misc ->
			Expires = sws_util:add_seconds_to(UtcNow, 30 * 24 * 60 * 60);
		_ ->
			Expires = undefined
	end,
	{Expires, ReqData, State}.

