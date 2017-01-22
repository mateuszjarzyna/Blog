-module(parser).

-export([run/3]).

-define(ACC_FUN, acctual_function).

run(ClassToRun, FunToRun, Args) ->
    put(?ACC_FUN, {ClassToRun, FunToRun}),
    {ArgsNames, Operations} = spl_manager:get_function(ClassToRun, FunToRun),
    parse_fun(Operations, ArgsNames, Args).

parse_fun(Operations, ArgsNames, ArgsToSet) ->
	Vars = set_args(ArgsNames, ArgsToSet, #{}),
    {FunctionResult, _LastVars} = parse(Operations, Vars),
    FunctionResult.

parse(Operations, Vars) when is_list(Operations) ->
    lists:foldl(fun(Operation, {_LastRes, AccVars}) ->
                    parse(Operation, AccVars)
                end,
                {false, Vars},
                Operations
                );
parse({if_else, Expression, WhenTrue, WhenFalse}, Vars) ->
    {ParsedExpression, Vars2} = parse(Expression, Vars),
	case ParsedExpression of
		true -> parse(WhenTrue, Vars2);
		false -> parse(WhenFalse, Vars2)
	end;
parse({greater_than, Left, Right}, Vars) ->
    {ParsedLeft, Vars2} = parse(Left, Vars),
    {ParsedRight, Vars3} = parse(Right, Vars2),
    Value = ParsedLeft > ParsedRight,
    {Value, Vars3};
parse({call_self_function, FunName, Args}, Vars) ->
    {AccClass, AccFun} = get(?ACC_FUN),
    RunRes = run(AccClass, FunName, Args),
    put(?ACC_FUN, {AccClass, AccFun}),
    {RunRes, Vars};
parse({assign_variable, Name, Value}, Vars) ->
    {Parsed, Vars2} = parse(Value, Vars),
    Vars3 = assign_variable(Name, Parsed, Vars2),
    {true, Vars3};
parse({variable, Name}, Vars) ->
    {get_variable(Name, Vars), Vars};
parse({string, String}, Vars) ->
    {String, Vars};
parse({number, Number}, Vars) ->
    {Number, Vars};
parse(UnknownOp, Vars) ->
	io:format("Unknown operation ~p\n", [UnknownOp]),
    {false, Vars}.

set_args([] = _FunArgsNames, [] = _ArgsToSet, Vars) ->
	Vars;
set_args([Name | RestFunArgsNames], [Value | RestArgsToSet], Vars) ->
	Vars2 = assign_variable(Name, Value, Vars),
	set_args(RestFunArgsNames, RestArgsToSet, Vars2).

assign_variable(Name, Value, Vars) ->
	maps:put(Name, Value, Vars).

get_variable(Name, Vars) ->
    maps:get(Name, Vars, undefined).