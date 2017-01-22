-module(parser).

-export([run/3]).

-define(FUN_VARS(F), list_to_atom(lists:flatten(io_lib:format("function_~p_variables", [F])))).
-define(FUNS, functions).
-define(ACC_FUN, accctual_function).

run(FileName, FunToRun, Args) ->
	{_ClassName, Functions} = spl:file(FileName),
	put(?FUNS, #{}),
	parse_functions(Functions),
        {ArgsNames, Operations} = get_function(FunToRun),
        parse_fun(FunToRun, Operations, ArgsNames, Args).

parse_fun(FunName, Operations, ArgsNames, ArgsToSet) ->
	put(?ACC_FUN, FunName),
        put(?FUN_VARS(get(?ACC_FUN)), #{}),
	set_args(ArgsNames, ArgsToSet),
	OperationsResults = parse(Operations),
	lists:last(OperationsResults).

parse(Operations) when is_list(Operations) ->
	[parse(Op) || Op <- Operations];
parse({if_else, Expression, WhenTrue, WhenFalse}) ->
	ExprRes = parse(Expression),
	case ExprRes of
		true -> parse(WhenTrue);
		false -> parse(WhenFalse)
	end;
parse({greather_than, Left, Right}) ->
	parse(Left) > parse(Right);
parse({assign_variable, Name, Value}) ->
	assign_variable(Name, parse(Value)),
	true;
parse({variable, Name}) ->
	get_variable(Name);
parse({string, String}) ->
	String;
parse({number, Number}) ->
	Number;
parse(UnknowOp) ->
	io:format("Unknown operation ~p\n", [UnknowOp]).

parse_functions(Functions) ->
	[add_function(Name, ArgsNames, Operations) || {Name, ArgsNames, Operations} <- Functions].
add_function(Name, ArgsNames, Operations) ->
	Funs = get(?FUNS),
	Funs2 = maps:put(Name, {ArgsNames, Operations}, Funs),
	put(?FUNS, Funs2).
get_function(FunName) ->
	Funs = get(?FUNS),
	maps:get(FunName, Funs).

set_args([] = _FunArgsNames, [] = _ArgsToSet) ->
	done;
set_args([Name | RestFunArgsNames], [Value | RestArgsToSet]) ->
	assign_variable(Name, Value),
	set_args(RestFunArgsNames, RestArgsToSet).

assign_variable(Name, Value) ->
	Vars = get(?FUN_VARS(get(?ACC_FUN))),
	Vars2 = maps:put(Name, Value, Vars),
	put(?FUN_VARS(get(?ACC_FUN)), Vars2).

get_variable(Name) ->
	Vars = get(?FUN_VARS(get(?ACC_FUN))),
	maps:get(Name, Vars, undefined).
