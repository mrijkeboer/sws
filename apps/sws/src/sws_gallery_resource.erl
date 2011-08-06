%% -------------------------------------------------------------------
%% sws_gallery_resource.erl - SWS photo gallery resource module
%% 
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS photo gallery resource module
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
-module(sws_gallery_resource).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		init/1,
		service_available/2,
		content_types_provided/2,
		encodings_provided/2,
		resource_exists/2,
		to_binary/2,
		to_html/2,
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
%%				{ok, State}
%% @doc Initialize resource.
%% @end
%% -------------------------------------------------------------------
init(_State) -> 
  {ok, #state{}}.


%% -------------------------------------------------------------------
%% @spec service_available(ReqData, State) ->
%%				{Availablility, ReqData, State}
%% @doc Check if this service is available.
%% @end
%% -------------------------------------------------------------------
service_available(ReqData, State) ->
	Host = sws_util:get_host(ReqData),
	Uri = wrq:path(ReqData),
	Type = get_type(Uri),
	FsPath = sws_util:gallery_fs_path(Host, Uri),
  {true, ReqData, State#state{host=Host, uri=Uri, type=Type, fs_path=FsPath}}.


%% -------------------------------------------------------------------
%% @spec content_types_provided(ReqData, State) ->
%%				{[{MediaType, Handler}]}, ReqData, State}
%% @doc Set the content types that are provided by this resource.
%% @end
%% -------------------------------------------------------------------
content_types_provided(ReqData, State) ->
	content_types_provided(ReqData, State, State#state.type).


content_types_provided(ReqData, State, file) ->
	case State#state.mime of
		undefined ->
			Mime = webmachine_util:guess_mime(State#state.fs_path),
			{[{Mime, to_binary}], ReqData, State#state{mime=Mime}};
		Mime ->
			{[{Mime, to_binary}], ReqData, State}
	end;

content_types_provided(ReqData, State, directory) ->
	{[{"text/html", to_html}], ReqData, State}.


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
%% @doc Check if the requested resource exists.
%% @end
%% -------------------------------------------------------------------
resource_exists(ReqData, State) ->
	resource_exists(ReqData, State, State#state.type).


resource_exists(ReqData, State, file) ->
	case sws_util:file_readable(State#state.fs_path) of
		{true, FileInfo} ->
			{true, ReqData, State#state{file_info=FileInfo}};
		false ->
			{false, ReqData, State}
	end; 

resource_exists(ReqData, State, directory) ->
	case sws_util:dir_readable(State#state.fs_path) of
		{true, FileInfo} ->
			{true, ReqData, State#state{file_info=FileInfo}};
		false ->
			{false, ReqData, State}
	end.


%% -------------------------------------------------------------------
%% @spec to_binary(ReqData, State) ->
%%				{Content, ReqData, State}
%% @doc Return the content of the resource as binary data.
%% @end
%% -------------------------------------------------------------------
to_binary(ReqData, State) ->
	{ok, Content} = file:read_file(State#state.fs_path),
	{Content, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec to_html(ReqData, State) ->
%%				{Content, ReqData, State}
%% @doc Return the content of the resource as html data.
%% @end
%% -------------------------------------------------------------------
to_html(ReqData, State) ->
	TemplatePath = sws_util:template_fs_path(State#state.host),
	TemplateFile = sws_config:gallery_template(),
	TemplateFilePath = filename:join([TemplatePath, TemplateFile]),
	erlydtl:compile(TemplateFilePath, template, [{doc_root, TemplatePath}]),

	FsPath = State#state.fs_path,
	Thumbs = filelib:wildcard(FsPath ++ "/*-thumb.{jpg,JPG,jpeg,JPEG,png,PNG}"),
	Images = lists:map(fun thumb_to_image_tuple/1, Thumbs),

	Uri = State#state.uri,
	Title = get_title(Uri),
	{ok, Content} = template:render([{uri, Uri}, {title, Title}, {images, Images}]),
	{Content, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec expires(ReqData, State) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}, ReqData, State}
%% @doc Set the expire date of the resource (file).
%% @end
%% -------------------------------------------------------------------
expires(ReqData, State) ->
	expires(ReqData, State, State#state.type).


expires(ReqData, State, file) ->
	UtcNow = calendar:universal_time(),
  ExpireTime = sws_config:file_expire_time(),
	Expires = sws_util:add_seconds_to(UtcNow, ExpireTime),
	{Expires, ReqData, State};

expires(ReqData, State, directory) ->
	UtcNow = calendar:universal_time(),
  ExpireTime = sws_config:page_expire_time(),
	Expires = sws_util:add_seconds_to(UtcNow, ExpireTime),
	{Expires, ReqData, State}.


%% -------------------------------------------------------------------
%% @spec get_type(Uri) ->
%%				directory |
%%				file
%% @doc Get type of the Uri.
%% @end
%% -------------------------------------------------------------------
get_type(Uri) ->
	case lists:last(Uri) of
		$/ ->
			directory;
		_ ->
			file
	end.
	

%% -------------------------------------------------------------------
%% @spec get_title(Uri) ->
%%				Title |
%%				[]
%% @doc Get the title from the Uri.
%% @end
%% -------------------------------------------------------------------
get_title(Uri) ->
	WithoutGallery = re:replace(Uri, "/gallery/", ""),
	WithoutSlashes = re:replace(WithoutGallery, "/", ""),
	WithoutUnderscores = re:replace(WithoutSlashes, "_", " ", [{return, list}]),
	Stripped = string:strip(WithoutUnderscores),
	Stripped.


%% -------------------------------------------------------------------
%% @spec thumb_to_image_tuple(ThumbPath) ->
%%				{image, ThumbImageName, LargeImageName}
%% @doc Convert the thumb filename to an images tuple.
%% @end
%% -------------------------------------------------------------------
thumb_to_image_tuple(ThumbPath) ->
  ThumbImageName = filename:basename(ThumbPath),
	ThumbPos = string:str(ThumbImageName, "-thumb."),
	FirstPart = string:substr(ThumbImageName, 1, ThumbPos - 1),
  Extention = filename:extension(ThumbImageName),
  LargeImageName = lists:concat([FirstPart, "-large", Extention]),
	{image, ThumbImageName, LargeImageName}.

