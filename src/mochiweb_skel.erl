-module(mochiweb_skel).
-export([skelcopy/2]).


-include_lib("kernel/include/file.hrl").

%% External API

skelcopy(DestDir, Name) ->
    ok = ensuredir(DestDir),
    LDst = case length(filename:dirname(DestDir)) of
               1 -> %% handle case when dirname returns "/"
                   0;
               N ->
                   N + 1
           end,
    skelcopy(src(), DestDir, Name, LDst),
    DestLink = filename:join([DestDir, Name, "deps", "mochiweb-src"]),
    ok = filelib:ensure_dir(DestLink),
    %
    case os:type() of
        {win32,_} ->
            {ok, Cwd} = file:get_cwd(),
            mk_win_dir_syslink(Name, "mochiweb", Cwd ++ "/../"),
            mk_bat_file(Name, Cwd);
        {unix,_} ->
            ok = file:make_symlink(
            filename:join(filename:dirname(code:which(?MODULE)), ".."),
            DestLink)
    end.

%% Internal API

%% @doc Make symbolik link in current directory on windows vista or highter
mk_win_dir_syslink(ProjectName, LinkName,DestLink) ->
    %io:format("~nname:~p~ntarget:~p~n~n", [LinkName, DestLink]),
    S = (list_to_atom("cd "++ ProjectName ++ "//deps" ++ "& mklink /D " ++ LinkName ++ " " ++ "\"" ++ DestLink ++ "\"")),
    %io:format("~n~p~n", [S]),
    os:cmd(S),
    ok.

%% @doc make .bat file to start dev server on windows
mk_bat_file(ProjectName, Cwd) ->
    Name = "start-dev.bat",
    Content = "make \n"
"start werl -pa ebin deps/*/ebin -boot start_sasl -s " ++ ProjectName ++ " -s \n"
"reloader ",
    file:set_cwd(Cwd ++ "//" ++ ProjectName),
    {ok, IODevice} = file:open(Name, [write]), file:write(IODevice, Content), file:close(IODevice),
    ok.

src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    Dest = re:replace(filename:basename(Src), skel(), Name,
                      [global, {return, list}]),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
            case filename:basename(Src) of
                "ebin" ->
                    ok;
                _ ->
                    {ok, Files} = file:list_dir(Src),
                    io:format("~s/~n", [EDst]),
                    lists:foreach(fun ("." ++ _) -> ok;
                                      (F) ->
                                          skelcopy(filename:join(Src, F),
                                                   Dir,
                                                   Name,
                                                   LDst)
                                  end,
                                  Files),
                        ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            S = re:replace(binary_to_list(B), skel(), Name,
                           [{return, list}, global]),
            ok = file:write_file(OutFile, list_to_binary(S)),
            ok = file:write_file_info(OutFile, #file_info{mode=Mode}),
            io:format("    ~s~n", [filename:basename(Src)]),
            ok;
        {ok, _} ->
            io:format("ignored source file: ~p~n", [Src]),
            ok
    end.

ensuredir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        E ->
            E
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
