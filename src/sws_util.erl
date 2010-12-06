%% -------------------------------------------------------------------
%% sws_util.erl - SWS utilities module
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
-module(sws_util).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([
		add_seconds_to/2,
		file_exists/1,
		file_readable/1,
		page_full_path/1, 
		static_full_path/2,
		template_full_path/0
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
%% @spec file_exists(FullPath) ->
%%				true |
%%				false
%% @doc Check if the specified file exists.
%% @end
%% -------------------------------------------------------------------
file_exists(FullPath) ->
	filelib:is_regular(FullPath).


%% -------------------------------------------------------------------
%% @spec file_readable(FullPath) ->
%%				{true, FileInfo} |
%%				false
%% @doc Check if the specified file exists and is readable.
%% @end
%% -------------------------------------------------------------------
file_readable(FullPath) ->
	case file_exists(FullPath) of
		true ->
			case file:read_file_info(FullPath) of
				{ok, FileInfo} ->
					case FileInfo#file_info.access of
						read ->
							{true, FileInfo};
						read_write ->
							{true, FileInfo};
						_ ->
							false
					end;
				_ ->
					false
			end;
		_ ->
			false
	end.


%% -------------------------------------------------------------------
%% @spec page_full_path(Uri) ->
%%				FullPath |
%%				undefined
%% @doc Get the full path to the page for the specified uri.
%% @end
%% -------------------------------------------------------------------
page_full_path([]) ->
	page_full_path("home");
page_full_path(undefined) ->
	page_full_path("home");
page_full_path("/") ->
	page_full_path("home");
page_full_path(Page) when is_list(Page) ->
	case string:substr(Page, string:len(Page)) of
		"/" ->
			full_path("pages", string:concat(Page, "index.html"));
		_ ->
			full_path("pages", string:concat(Page, ".html"))
	end.


%% -------------------------------------------------------------------
%% @spec static_full_path(Uri) ->
%%				FullPath |
%%				undefined
%% @doc Get the full path to the static file for the specified uri.
%% @end
%% -------------------------------------------------------------------
static_full_path([], _Type) ->
	undefined;
static_full_path(undefined, _Type) ->
	undefined;
static_full_path(Uri, Type) when is_list(Uri), is_atom(Type) ->
	case Type of
		file ->
			full_path(Uri);
		lib ->
			full_path(Uri);
		misc ->
			full_path("misc", Uri);
		_ ->
			undefined
	end.


%% -------------------------------------------------------------------
%% @spec template_full_path() ->
%%				FullPath
%% @doc Get the full path to the templates directory.
%% @end
%% -------------------------------------------------------------------
template_full_path() ->
	full_path("templates").


%% ===================================================================
%% Internal functions
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec full_path(Uri) ->
%%				FullPath |
%%				undefined
%% @doc Get the full path for the specified uri.
%% @end
%% -------------------------------------------------------------------
full_path(Uri) when is_list(Uri) ->
	case cleanup(Uri) of
		undefined ->
			undefined;
		CleanUri ->
			filename:join([filename:absname(""), priv, CleanUri])
	end.


%% -------------------------------------------------------------------
%% @spec full_path(Subdir, Uri) ->
%%				FullPath |
%%				undefined
%% @doc Get the full path for the specified sub directory and uri.
%% @end
%% -------------------------------------------------------------------
full_path(SubDir, Uri) when is_list(SubDir), is_list(Uri) ->
	case cleanup(SubDir) of
		undefined ->
			undefined;
		CleanSubDir ->
			case cleanup(Uri) of
				undefined ->
					undefined;
				CleanUri ->
					filename:join([filename:absname(""), priv, CleanSubDir, CleanUri])
			end
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


%% ===================================================================
%% Test functions
%% ===================================================================

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

add_seconds_to_test() ->
	T1 = {{2010, 12, 3}, {20,0,0}},
	T2 = add_seconds_to(T1, 60),
	?assert({{2010, 12, 3}, {20,1,0}} =:= T2).


file_exists_test() ->
	ThisFile = code:which(?MODULE),
	?assert(file_exists(ThisFile)),
	NonExistingFile = string:concat(ThisFile, ".erl"),
	?assertNot(file_exists(NonExistingFile)).


file_readable_test() ->
	ThisFile = code:which(?MODULE),
	?assertMatch({true, _}, file_readable(ThisFile)),
	?assert(file_exists("/etc/shadow")),
	?assertNot(file_readable("/etc/shadow")).


page_full_path_test() ->
	CWD = filename:absname(""),
	?assertEqual(filename:join([CWD, priv, "pages/home.html"]), page_full_path([])),
	?assertEqual(filename:join([CWD, priv, "pages/home.html"]), page_full_path(undefined)),
	?assertEqual(filename:join([CWD, priv, "pages/home.html"]), page_full_path("")),
	?assertEqual(filename:join([CWD, priv, "pages/home.html"]), page_full_path("/")),
	?assertEqual(filename:join([CWD, priv, "pages/home.html"]), page_full_path("/home")),
	?assertEqual(filename:join([CWD, priv, "pages/docs.html"]), page_full_path("/docs")),
	?assertEqual(filename:join([CWD, priv, "pages/docs/index.html"]), page_full_path("/docs/")),
	?assertEqual(filename:join([CWD, priv, "pages/doc/toc.html"]), page_full_path("/doc/toc")),
	?assertEqual(undefined, page_full_path("/../home")),
	?assertEqual(undefined, page_full_path("../home")).


static_full_path_test() ->
	CWD = filename:absname(""),
	?assertEqual(filename:join([CWD, priv, "lib/css/site.css"]), static_full_path("/lib/css/site.css", lib)),
	?assertEqual(filename:join([CWD, priv, "lib/css/site.css"]), static_full_path("lib/css/site.css", lib)),
	?assertEqual(filename:join([CWD, priv, "lib/image/logo.jpg"]), static_full_path("/lib/image/logo.jpg", lib)),
	?assertEqual(filename:join([CWD, priv, "lib/image/logo.jpg"]), static_full_path("lib/image/logo.jpg", lib)),
	?assertEqual(filename:join([CWD, priv, "lib/js/test.js"]), static_full_path("/lib/js/test.js", lib)),
	?assertEqual(filename:join([CWD, priv, "lib/js/test.js"]), static_full_path("lib/js/test.js", lib)),
	?assertEqual(filename:join([CWD, priv, "files/test.jpg"]), static_full_path("/files/test.jpg", file)),
	?assertEqual(filename:join([CWD, priv, "files/test.jpg"]), static_full_path("files/test.jpg", file)),
	?assertEqual(filename:join([CWD, priv, "files/doc/doc.doc"]), static_full_path("/files/doc/doc.doc", file)),
	?assertEqual(filename:join([CWD, priv, "files/doc/doc.doc"]), static_full_path("files/doc/doc.doc", file)),
	?assertEqual(filename:join([CWD, priv, "misc/favicon.ico"]), static_full_path("/favicon.ico", misc)),
	?assertEqual(filename:join([CWD, priv, "misc/favicon.ico"]), static_full_path("favicon.ico", misc)),
	?assertEqual(filename:join([CWD, priv, "misc/robots.txt"]), static_full_path("/robots.txt", misc)),
	?assertEqual(filename:join([CWD, priv, "misc/robots.txt"]), static_full_path("robots.txt", misc)),
	?assertEqual(undefined, static_full_path("/../files/test.jpg", file)),
	?assertEqual(undefined, static_full_path("/files/test.jpg", foobar)).


template_full_path_test() ->
	CWD = filename:absname(""),
	?assertEqual(filename:join([CWD, priv, "templates"]), template_full_path()).


full_path_test() ->
	CWD = filename:absname(""),
	% full_path/1
	?assertEqual(filename:join([CWD, priv, "home"]), full_path("/home")),
	?assertEqual(filename:join([CWD, priv, "home"]), full_path("home")),
	?assertEqual(filename:join([CWD, priv, "home/dir"]), full_path("/home/dir")),
	?assertEqual(undefined, full_path("/../home")),
	?assertEqual(undefined, full_path("../home")),
	% full_path/2
	?assertEqual(filename:join([CWD, priv, "files/home"]), full_path("files", "/home")),
	?assertEqual(filename:join([CWD, priv, "files/home"]), full_path("files", "home")),
	?assertEqual(filename:join([CWD, priv, "files/home/dir"]), full_path("files", "/home/dir")),
	?assertEqual(undefined, full_path("files", "/../home")),
	?assertEqual(undefined, full_path("files", "../home")).


cleanup_test() ->
	?assertEqual("", cleanup("/")),
	?assertEqual("home", cleanup("/home")),
	?assertEqual("./home", cleanup("/./home")),
	?assertEqual(undefined, cleanup("/../home")),
	?assertEqual(undefined, cleanup("/home/../../bla")).

-endif.
