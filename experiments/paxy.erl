-module(paxy).
-export([start/1, stop/0, stop/1, crash/1, start_dist/3]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(MAGENTA, {255,0, 255}).
-define(CYAN, {0,255,255}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  %AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie", "Burns", "Apu", "Abraham", "Ned", "Moe"],
  %AccRegister = [homer, marge, bart, lisa, maggie, burns, apu, abraham, ned, moe],
  %ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}, {"ZoidBerg", ?MAGENTA}, {"Zapp", ?CYAN}],
  %PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}, {zoidberg, ?MAGENTA}, {zapp, ?CYAN}],
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() ->
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.

start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  %lists:map(fun(E) ->stop(E) end, AccRegister),
  stop(homer),
  stop(marge),
  stop(bart),
  stop(lisa),
  stop(maggie),
  %stop(burns),
  %stop(apu),
  %stop(abraham),
  %stop(ned),
  %stop(moe),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:delete(Name),
      Pid ! stop
  end.

crash(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:open(Name),
      {_, _, _, Pn} = pers:read(Name),
      Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
      pers:close(Name),
      unregister(Name),
      exit(Pid, "crash"),
      timer:sleep(3000),
      register(Name, acceptor:start(Name, na))
  end.

start_dist(Sleep, AccNode, ProNode) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      AccRegisterDist = lists:map(fun(Acc) -> {Acc, AccNode} end, AccRegister),
      start_acceptors(AccIds, AccRegister),
      io:format("[Paxy] AccRegisterDist: ~w~n", [AccRegisterDist]),
      spawn(ProNode, fun() ->
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegisterDist, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.