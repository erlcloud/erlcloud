-module(erlcloud_tests).

-include_lib("eunit/include/eunit.hrl").

erlcloud_test_() ->
    {foreach,
        fun () ->
            true = code:add_path(get_test_dir())
        end,
        [{<<"Single .hrl inclusion: check that 1 .hrl inclusion is enough, when the .hrl is required.">>,
          fun include_one_by_one/0},
         {<<"Full .hrl inclusion: check that -include()'ing all .hrl files is not an issue.">>,
          fun include_all/0}]
    }.

include_one_by_one() ->
    {ok, HRLFiles} = file:list_dir(get_include_dir()),

    lists:foreach(
        fun (HRLFileStr) ->
            HRLFile = list_to_binary(HRLFileStr),
            TestFileWithExtension = binary:replace(HRLFile, <<".hrl">>, <<"_include_test.erl">>, []),
            TestFileWithoutExtension = binary:replace(TestFileWithExtension, <<".erl">>, <<"">>, []),
            TestFileBeam = binary:replace(TestFileWithExtension, <<".erl">>, <<".beam">>, []),
            TestFileWithoutExtensionAtom = binary_to_atom(TestFileWithoutExtension, utf8),
            TestDir = list_to_binary(get_test_dir()),
            TempFilePath = <<"./", TestDir/binary, "/", TestFileWithExtension/binary>>,
            TempFilePathStr = binary_to_list(TempFilePath),
            ok = file:write_file(TempFilePath,
                                 <<"-module(", TestFileWithoutExtension/binary, ").\n",
                                   "\n",
                                   "-include(\"../include/", HRLFile/binary, "\").">>),
            {ok, TestFileWithoutExtensionAtom} = compile:file(TempFilePathStr),
            {module, TestFileWithoutExtensionAtom} = code:load_file(TestFileWithoutExtensionAtom),
            ok = file:delete(TempFilePath),
            ok = file:delete(TestFileBeam)
        end,
        HRLFiles),
    ok.

include_all() ->
    {ok, HRLFiles} = file:list_dir(get_include_dir()),
    
    IncludeDirectivesStr = ["-include(\"../include/"++HRLFile++"\").\n"|| HRLFile <- HRLFiles],
    IncludeDirectives = list_to_binary(IncludeDirectivesStr),
    TestDir = list_to_binary(get_test_dir()),
    TempFilePath = <<"./", TestDir/binary, "/ercloud_include_all_test.erl">>,
    TempFilePathStr = binary_to_list(TempFilePath),
    ok = file:write_file(TempFilePath,
                         <<"-module(ercloud_include_all_test).\n",
                           "\n",
                           IncludeDirectives/binary>>),
    {ok, ercloud_include_all_test} = compile:file(TempFilePathStr),
    {module, ercloud_include_all_test} = code:load_file(ercloud_include_all_test),
    ok = file:delete(TempFilePath),
    ok = file:delete("ercloud_include_all_test.beam"),
    ok.

get_test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    case filename:basename(Cwd) of
        ".eunit" -> % rebar 2
            "../test/";
        _ -> % rebar3
            "./test/"
    end.

get_include_dir() ->
    {ok, Cwd} = file:get_cwd(),
    case filename:basename(Cwd) of
        ".eunit" -> % rebar 2
            "../include";
        _ -> % rebar3
            "include"
    end.
