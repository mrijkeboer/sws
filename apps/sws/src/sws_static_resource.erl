%% -------------------------------------------------------------------
%% sws_static_resource.erl - SWS static files resource module
%% 
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2010, 2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS static files resource module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010, 2011 Martijn Rijkeboer
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
    service_available/2,
    content_types_provided/2,
    encodings_provided/2,
    resource_exists/2,
    to_binary/2,
    last_modified/2,
    expires/2
  ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").


%% State record.
-record(state, {host, uri, type, fs_path, mime, file_info}).


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec init(State) ->
%%        {ok, State}
%% @doc Initialize resource.
%% @end
%% -------------------------------------------------------------------
init(State) -> 
  case proplists:get_value(type, State) of
    undefined ->
      {ok, #state{type=file}};
    Type ->
      {ok, #state{type=Type}}
  end.


%% -------------------------------------------------------------------
%% @spec service_available(ReqData, State) ->
%%        {Availablility, ReqData, State}
%% @doc Check if this service is available.
%% @end
%% -------------------------------------------------------------------
service_available(ReqData, State) ->
  Host = sws_util:get_host(ReqData),
  Uri = wrq:path(ReqData),
  Type = State#state.type,
  FsPath = sws_util:static_fs_path(Host, Uri, Type),
  {true, ReqData, State#state{host=Host, uri=Uri, fs_path=FsPath}}.


%% -------------------------------------------------------------------
%% @spec content_types_provided(ReqData, State) ->
%%        {[{MediaType, Handler}]}, ReqData, State}
%% @doc Set the content types that are provided by this resource.
%% @end
%% -------------------------------------------------------------------
content_types_provided(ReqData, State) ->
  case State#state.mime of
    undefined ->
      Mime = webmachine_util:guess_mime(State#state.fs_path),
      {[{Mime, to_binary}], ReqData, State#state{mime=Mime}};
    Mime ->
      {[{Mime, to_binary}], ReqData, State}
  end.


%% -------------------------------------------------------------------
%% @spec encodings_provided(ReqData, State) ->
%%        {[{"identity", fun(X) -> X end},
%%          {"gzip", fun(X) -> zlib:gzip(X) end}],
%%         ReqData, State}
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
%%        {true, ReqData, State} |
%%        {false, ReqData, State}
%% @doc Check if the requested resource exists (file).
%% @end
%% -------------------------------------------------------------------
resource_exists(ReqData, State) ->
  case sws_util:file_readable(State#state.fs_path) of
    {true, FileInfo} ->
      {true, ReqData, State#state{file_info=FileInfo}};
    false ->
      {false, ReqData, State}
  end.


%% -------------------------------------------------------------------
%% @spec to_binary(ReqData, State) ->
%%        {Content, ReqData, State}
%% @doc Return the content of the resource as binary data.
%% @end
%% -------------------------------------------------------------------
to_binary(ReqData, State) ->
  {ok, Content} = file:read_file(State#state.fs_path),
  {Content, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec last_modified(ReqData, State) ->
%%        {{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the last modified time of the resource (file).
%% @end
%% -------------------------------------------------------------------
last_modified(ReqData, State) ->
  FileInfo = State#state.file_info,
  {FileInfo#file_info.mtime, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec expires(ReqData, State) ->
%%        {{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the expire date of the resource (file).
%% @end
%% -------------------------------------------------------------------
expires(ReqData, State) ->
  UtcNow = calendar:universal_time(),
  ExpireTime = case State#state.type of
    file ->
      sws_config:file_expire_time();
    lib ->
      sws_config:lib_expire_time();
    misc ->
      sws_config:misc_expire_time();
    _ ->
      0
  end,
  Expires = sws_util:add_seconds_to(UtcNow, ExpireTime),
  {Expires, ReqData, State}.

