%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2017 9:24 AM
%%%-------------------------------------------------------------------
-module(spl_manager).
-author("mateusz").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([parse_class/1, get_function/2]).

-define(SERVER, ?MODULE).

-record(state, {
    classes
}).

parse_class(FileName) ->
    gen_server:call(?SERVER, {parse_class, FileName}).

get_function(ClassName, FunctionName) ->
    gen_server:call(?SERVER,  {get_function, ClassName, FunctionName}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{
        classes = #{}
    }}.

handle_call({parse_class, FileName}, _From, State = #state{classes = Classes}) ->
    {ClassName, Functions} = spl:file(FileName),
    ParsedFunctions = parse_functions(Functions),
    Classes2 = maps:put(ClassName, ParsedFunctions, Classes),
    {reply, ok, State#state{
        classes = Classes2
    }};
handle_call({get_function, Class, Function}, _From, State = #state{classes = Classes}) ->
    FunToReturn = get_function_fom_state(Class, Function, Classes),
    {reply, FunToReturn, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_functions(Functions) ->
    lists:foldl(fun({Name, ArgsNames, Operations}, FunsMap) ->
                    add_function(Name, ArgsNames, Operations, FunsMap)
            end, #{}, Functions).
add_function(Name, ArgsNames, Operations, Funs) ->
    maps:put(Name, {ArgsNames, Operations}, Funs).

get_function_fom_state(ClassName, FunName, Classes) ->
    Functions = maps:get(ClassName, Classes),
    maps:get(FunName, Functions).