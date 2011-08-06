%% -------------------------------------------------------------------
%% sws_util.erl - SWS utilities module
%% 
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2010, 2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc SWS utilities module
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
-module(sws_util).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		add_seconds_to/2,
    dir_readable/1,
		file_readable/1,
		gallery_fs_path/2,
		get_host/1,
		page_fs_path/2, 
		static_fs_path/3,
		template_fs_path/1
	]).

-include_lib("kernel/include/file.hrl").

%% -------------------------------------------------------------------
%% @spec add_seconds_to(Seconds) ->
%%				{{{YYYY,MM,DD}, {Hour,Min,Sec}}
%% @doc Add the specified number of seconds to the current time.
%% @end
%% -------------------------------------------------------------------
add_seconds_to({Date, Time}, Seconds) when is_integer(Seconds) ->
	GregorianSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
	NewSeconds = GregorianSeconds + Seconds,
	calendar:gregorian_seconds_to_datetime(NewSeconds).


%% -------------------------------------------------------------------
%% @spec dir_readable(FsPath) ->
%%				{true, FileInfo} |
%%				false
%% @doc Check if the specified dir exists and is readable.
%% @end
%% -------------------------------------------------------------------
dir_readable(FsPath) when is_list(FsPath) ->
  case filelib:is_dir(FsPath) of
		true ->
      readable(FsPath);
		false ->
			false
	end.


%% -------------------------------------------------------------------
%% @spec file_readable(FsPath) ->
%%				{true, FileInfo} |
%%				false
%% @doc Check if the specified file exists and is readable.
%% @end
%% -------------------------------------------------------------------
file_readable(FsPath) when is_list(FsPath) ->
  case filelib:is_regular(FsPath) of
		true ->
      readable(FsPath);
		false ->
			false
	end.


%% -------------------------------------------------------------------
%% @spec page_fs_path(Host, Uri) ->
%%				FsPath |
%%				undefined
%% @doc Get the path to the page for the Host and Uri.
%% @end
%% -------------------------------------------------------------------
page_fs_path(Host, []) ->
	page_fs_path(Host, "home");
page_fs_path(Host, undefined) ->
	page_fs_path(Host, "home");
page_fs_path(Host, "/") ->
	page_fs_path(Host, "home");
page_fs_path(Host, Page) when is_list(Page) ->
	case string:substr(Page, string:len(Page)) of
		"/" ->
			get_fs_path(Host, "pages", string:concat(Page, "index.html"));
		_ ->
			get_fs_path(Host, "pages", string:concat(Page, ".html"))
	end.


%% -------------------------------------------------------------------
%% @spec static_fs_path(Host, Uri, Type) ->
%%				FsPath |
%%				undefined
%% @doc Get the path to the static file for Host, Uri and Type.
%% @end
%% -------------------------------------------------------------------
static_fs_path(_Host, [], _Type) ->
	undefined;
static_fs_path(_Host, undefined, _Type) ->
	undefined;
static_fs_path(Host, Uri, Type) when is_list(Uri), is_atom(Type) ->
	case Type of
		file ->
			get_fs_path(Host, Uri);
		lib ->
			get_fs_path(Host, Uri);
		misc ->
			get_fs_path(Host, "misc", Uri);
		_ ->
			undefined
	end.


%% -------------------------------------------------------------------
%% @spec template_fs_path(Host) ->
%%				FsPath
%% @doc Get the path to the templates directory for Host.
%% @end
%% -------------------------------------------------------------------
template_fs_path(Host) ->
	get_fs_path(Host, "templates").


%% -------------------------------------------------------------------
%% @spec gallery_fs_path(Host) ->
%%				FsPath
%% @doc Get the path to the gallery for Host and Uri.
%% @end
%% -------------------------------------------------------------------
gallery_fs_path(Host, Uri) ->
	get_fs_path(Host, Uri).


%% ===================================================================
%% Internal functions
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec get_fs_path(Host, Uri) ->
%%				FsPath |
%%				undefined
%% @doc Get the filesystem path for the Host and Uri.
%% @end
%% -------------------------------------------------------------------
get_fs_path(Host, Uri) when is_list(Uri) ->
	case cleanup(Uri) of
		undefined ->
			undefined;
		CleanUri ->
			filename:join([get_fs_prefix(Host), CleanUri])
	end.


%% -------------------------------------------------------------------
%% @spec get_fs_path(Host, SubDir, Uri) ->
%%				FsPath |
%%				undefined
%% @doc Get the filesystem path for Host, SubDir and Uri.
%% @end
%% -------------------------------------------------------------------
get_fs_path(Host, SubDir, Uri) when is_list(SubDir), is_list(Uri) ->
	case cleanup(SubDir) of
		undefined ->
			undefined;
		CleanSubDir ->
			case cleanup(Uri) of
				undefined ->
					undefined;
				CleanUri ->
					filename:join([get_fs_prefix(Host), CleanSubDir, CleanUri])
			end
	end.


%% -------------------------------------------------------------------
%% @spec get_fs_prefix(Host) ->
%%				FsPrefix
%% @doc Get the filesystem prefix for the Host.
%% @end
%% -------------------------------------------------------------------
get_fs_prefix(Host) ->
	get_fs_prefix(Host, sws_config:virtual_hosting()).


%% -------------------------------------------------------------------
%% @spec get_fs_prefix(Host, VirtualHosting) ->
%%				FsPrefix
%% @doc Get the filesystem prefix for the Host and VirtualHosting.
%% @end
%% -------------------------------------------------------------------
get_fs_prefix(Host, true) ->
	filename:join([sws_config:www_root_path(), Host]);
get_fs_prefix(_Host, false) ->
	sws_config:www_root_path().


%% -------------------------------------------------------------------
%% @spec get_host(ReqData) ->
%%				Host |
%%				undefined
%% @doc Get the name of the host from the request data.
%% @end
%% -------------------------------------------------------------------
get_host(ReqData) ->
	HostString = case wrq:get_req_header("x-forwarded-host", ReqData) of
		undefined ->
			case wrq:get_req_header("x-forwarded-server", ReqData) of
				undefined ->
					case wrq:get_req_header("host", ReqData) of
						undefined ->
							undefined;
						Host ->
							Host
					end;
				ForwardedServer ->
					ForwardedServer
			end;
		ForwardedHost ->
			ForwardedHost
	end,
	get_host_part(HostString).


%% -------------------------------------------------------------------
%% @spec get_host_part(HostString) ->
%%				Host |
%%				undefined
%% @doc Get the name of the host from host string.
%% @end
%% -------------------------------------------------------------------
get_host_part(undefined) ->
	undefined;
get_host_part(HostString) ->
	case string:tokens(HostString, ":") of
		[Host, _Port] ->
			Host;
		[Host] ->
			Host;
		[] ->
			undefined
	end.


%% -------------------------------------------------------------------
%% @spec cleanup(Uri) ->
%%				CleanRelativeName |
%%				undefined
%% @doc Cleanup the specified uri.
%% @end
%% -------------------------------------------------------------------
cleanup(Uri) ->
	RelUri = case hd(Uri) of
		$/ -> tl(Uri);
		_ -> Uri
	end,
	mochiweb_util:safe_relative_path(RelUri).


%% -------------------------------------------------------------------
%% @spec readable(FsPath) ->
%%				{true, FileInfo} |
%%				false
%% @doc Check if the specified path is readable.
%% @end
%% -------------------------------------------------------------------
readable(FsPath) ->
  case file:read_file_info(FsPath) of
    {ok, FileInfo} ->
      case FileInfo#file_info.access of
        read ->
          {true, FileInfo};
        read_write ->
          {true, FileInfo};
        _ ->
          false
      end;
    {error, _Reason} ->
      false
  end.


%% ===================================================================
%% Test functions
%% ===================================================================

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

add_seconds_to_test() ->
	T1 = {{2010, 12, 3}, {20,0,0}},
	T2 = add_seconds_to(T1, 60),
	?assert({{2010, 12, 3}, {20,1,0}} =:= T2).


dir_readable_test() ->
  ?assertMatch({true, _}, dir_readable("/tmp")),
	?assert(filelib:is_dir("/root")),
	?assertNot(file_readable("/root")).


file_readable_test() ->
	ThisFile = code:which(?MODULE),
	?assertMatch({true, _}, file_readable(ThisFile)),
	?assert(filelib:is_regular("/etc/shadow")),
	?assertNot(file_readable("/etc/shadow")).


page_fs_path_test() ->
	?assertEqual(filename:join([priv, "pages/home.html"]), page_fs_path("", [])),
	?assertEqual(filename:join([priv, "pages/home.html"]), page_fs_path("", undefined)),
	?assertEqual(filename:join([priv, "pages/home.html"]), page_fs_path("", "")),
	?assertEqual(filename:join([priv, "pages/home.html"]), page_fs_path("", "/")),
	?assertEqual(filename:join([priv, "pages/home.html"]), page_fs_path("", "/home")),
	?assertEqual(filename:join([priv, "pages/docs.html"]), page_fs_path("", "/docs")),
	?assertEqual(filename:join([priv, "pages/docs/index.html"]), page_fs_path("", "/docs/")),
	?assertEqual(filename:join([priv, "pages/doc/toc.html"]), page_fs_path("", "/doc/toc")),
	?assertEqual(undefined, page_fs_path("", "/../home")),
	?assertEqual(undefined, page_fs_path("", "../home")).


static_fs_path_test() ->
	?assertEqual(filename:join([priv, "lib/css/site.css"]), static_fs_path("", "/lib/css/site.css", lib)),
	?assertEqual(filename:join([priv, "lib/css/site.css"]), static_fs_path("", "lib/css/site.css", lib)),
	?assertEqual(filename:join([priv, "lib/image/logo.jpg"]), static_fs_path("", "/lib/image/logo.jpg", lib)),
	?assertEqual(filename:join([priv, "lib/image/logo.jpg"]), static_fs_path("", "lib/image/logo.jpg", lib)),
	?assertEqual(filename:join([priv, "lib/js/test.js"]), static_fs_path("", "/lib/js/test.js", lib)),
	?assertEqual(filename:join([priv, "lib/js/test.js"]), static_fs_path("", "lib/js/test.js", lib)),
	?assertEqual(filename:join([priv, "files/test.jpg"]), static_fs_path("", "/files/test.jpg", file)),
	?assertEqual(filename:join([priv, "files/test.jpg"]), static_fs_path("", "files/test.jpg", file)),
	?assertEqual(filename:join([priv, "files/doc/doc.doc"]), static_fs_path("", "/files/doc/doc.doc", file)),
	?assertEqual(filename:join([priv, "files/doc/doc.doc"]), static_fs_path("", "files/doc/doc.doc", file)),
	?assertEqual(filename:join([priv, "misc/favicon.ico"]), static_fs_path("", "/favicon.ico", misc)),
	?assertEqual(filename:join([priv, "misc/favicon.ico"]), static_fs_path("", "favicon.ico", misc)),
	?assertEqual(filename:join([priv, "misc/robots.txt"]), static_fs_path("", "/robots.txt", misc)),
	?assertEqual(filename:join([priv, "misc/robots.txt"]), static_fs_path("", "robots.txt", misc)),
	?assertEqual(undefined, static_fs_path("", "/../files/test.jpg", file)),
	?assertEqual(undefined, static_fs_path("", "/files/test.jpg", foobar)).


template_fs_path_test() ->
	?assertEqual(filename:join([priv, "templates"]), template_fs_path("")).


gallery_fs_path_test() ->
	?assertEqual(filename:join([priv, "gallery", "flora"]), gallery_fs_path("", "/gallery/flora")).


get_fs_path_test() ->
	% get_fs_path/2
	?assertEqual(filename:join([priv, "home"]), get_fs_path("", "/home")),
	?assertEqual(filename:join([priv, "home"]), get_fs_path("", "home")),
	?assertEqual(filename:join([priv, "home/dir"]), get_fs_path("", "/home/dir")),
	?assertEqual(undefined, get_fs_path("", "/../home")),
	?assertEqual(undefined, get_fs_path("", "../home")),
	% get_fs_path/3
	?assertEqual(filename:join([priv, "files/home"]), get_fs_path("", "files", "/home")),
	?assertEqual(filename:join([priv, "files/home"]), get_fs_path("", "files", "home")),
	?assertEqual(filename:join([priv, "files/home/dir"]), get_fs_path("", "files", "/home/dir")),
	?assertEqual(undefined, get_fs_path("", "files", "/../home")),
	?assertEqual(undefined, get_fs_path("", "files", "../home")).


get_fs_prefix_test() ->
	% get_fs_prefix/2
	?assertEqual("priv", get_fs_prefix("localhost", false)),
	?assertEqual("priv/localhost", get_fs_prefix("localhost", true)).


get_host_test() ->
	?assertEqual("host", get_host(new_wrq([{"Host", "host:8000"}]))),
	?assertEqual("fhost", get_host(new_wrq([{"X-Forwarded-Host", "fhost:8000"}]))),
	?assertEqual("fserver", get_host(new_wrq([{"X-Forwarded-Server", "fserver:8000"}]))),
	?assertEqual("fhost", get_host(new_wrq([
					{"X-Forwarded-Server", "fserver:8000"},
					{"X-Forwarded-Host", "fhost:8000"},
					{"Host", "host:8000"}
				]))).


get_host_part_test() ->
	?assertEqual(undefined, get_host_part(undefined)),
	?assertEqual("localhost", get_host_part("localhost")),
	?assertEqual("localhost", get_host_part("localhost:8000")).


cleanup_test() ->
	?assertEqual("", cleanup("/")),
	?assertEqual("home", cleanup("/home")),
	?assertEqual("./home", cleanup("/./home")),
	?assertEqual(undefined, cleanup("/../home")),
	?assertEqual(undefined, cleanup("/home/../../bla")).


new_wrq(Headers) ->
	wrq:create("GET", {1, 1}, "/", mochiweb_headers:from_list(Headers)).

-endif.
