-module(jcuzdeasm).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/jcuzdeasm.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(StartArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    asm2code(Args),
    self() ! quit,
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(quit, State) ->
    exit(normal),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    halt(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

asm2code(FileList) ->
    lists:map(fun(File) -> asm2code_1(File) end, FileList).

asm2code_1(File) ->
    File1 = filename:join([element(2,file:get_cwd()), File]),
    file:write_file(to_java_file_name(File1), list_to_binary(javasm:to_code(File1))).

to_java_file_name(File) ->
    FN = string:join([filename:rootname(File), ".java"], ""),
    io:fwrite("[generate] ~s~n", [FN]),
    FN.
