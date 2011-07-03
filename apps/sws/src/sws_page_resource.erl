%% -------------------------------------------------------------------
%% sws_page_resource.erl - SWS webpages resource module
%% 
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010,2011 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS webpages resource module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010,2011 Martijn P. Rijkeboer
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
-module(sws_page_resource).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		init/1,
		encodings_provided/2,
		resource_exists/2,
		to_html/2,
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
	{ok, State}.


%% -------------------------------------------------------------------
%% @spec encodings_provided(ReqData, State) ->
%%				{[{"identity", fun(X) -> X end},
%%				  {"gzip", fun(X) -> zlib:gzip(X) end}],
%%				 ReqData, State}
%% @doc Return encodings provided.
%% @end
%% -------------------------------------------------------------------
encodings_provided(ReqData, State) ->
	{
		[
			{"identity", fun(X) -> X end},
			{"gzip", fun(X) -> zlib:gzip(X) end}
		],
		ReqData, State
	}.


%% -------------------------------------------------------------------
%% @spec resource_exists(ReqData, State) ->
%%				{true, ReqData, State} |
%%				{false, ReqData, State}
%% @doc Check if the requested resource exists (page).
%% @end
%% -------------------------------------------------------------------
resource_exists(ReqData, State) ->
	Host = sws_util:get_host(wrq:get_req_header(host, ReqData)),
	Uri = wrq:path(ReqData),
	FsPath = sws_util:page_fs_path(Host, Uri),
	case sws_util:file_readable(FsPath) of
		{true, FileInfo} ->
			State1 = State ++ [{host, Host}, {uri, Uri}, {fs_path, FsPath}, {file_info, FileInfo}],
			{true, ReqData, State1};
		_ ->
			{false, ReqData, State}
	end.


%% -------------------------------------------------------------------
%% @spec to_html(ReqData, State) ->
%%				{Content, ReqData, State}
%% @doc Return the content of the resource as HTML.
%% @end
%% -------------------------------------------------------------------
to_html(ReqData, State) ->
	Host = proplists:get_value(host, State),
	Uri = proplists:get_value(uri, State),
	FsPath = proplists:get_value(fs_path, State),
	TemplatePath = sws_util:template_fs_path(Host),
	erlydtl:compile(FsPath, template, [{doc_root, TemplatePath}]),
	{ok, Content} = template:render([{uri, Uri}]),
	{Content, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec last_modified(ReqData, State) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the last modified time of the resource (page).
%% @end
%% -------------------------------------------------------------------
last_modified(ReqData, State) ->
	FileInfo = proplists:get_value(file_info, State),
	{FileInfo#file_info.mtime, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec expires(ReqData, State) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the expire date of the resource (page).
%% @end
%% -------------------------------------------------------------------
expires(ReqData, State) ->
	UtcNow = calendar:universal_time(),
	Expires = sws_util:add_seconds_to(UtcNow, sws_config:page_expire_time()),
	{Expires, ReqData, State}.

