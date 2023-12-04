-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n",
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n",
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  case collect(Quorum, Round, MaxVoted, Proposal) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(N, Round, MaxVoted, Proposal) ->
  collect(N, N, Round, MaxVoted, Proposal).
collect(0, _, _, _, Proposal) ->
  {accepted, Proposal};
collect(_, 0, _, _, _) ->
  abort;
collect(NumPromise, NumSorry, Round, MaxVoted, Proposal) ->
  receive
    {promise, Round, _, na} ->
      collect(NumPromise-1, NumSorry, Round, MaxVoted, Proposal);
    {promise, Round, Voted, Value} ->
      case order:gr(Voted, MaxVoted) of
        true ->
          collect(NumPromise-1, NumSorry, Round, Voted, Value);
        false ->
          collect(NumPromise-1, NumSorry, Round, MaxVoted, Proposal)
      end;
    {promise, _, _,  _} ->
      collect(NumPromise, NumSorry, Round, MaxVoted, Proposal);
    {sorry, {prepare, Round}} ->
      collect(NumPromise, NumSorry-1, Round, MaxVoted, Proposal);
    {sorry, _} ->
      collect(NumPromise, NumSorry-1, MaxVoted, Proposal)
  after ?timeout ->
    abort
  end.

vote(N, Round) ->
  vote(N, N, Round).
vote(0, _, _) ->
  ok;
vote(_, 0, _) ->
  abort;
vote(NumVote, NumSorry, Round) ->
  receive
    {vote, Round} ->
      vote(NumVote-1, NumSorry, Round);
    {vote, _} ->
      vote(NumVote, NumSorry, Round);
    {sorry, {accept, Round}} ->
      vote(NumVote, NumSorry-1, Round);
    {sorry, _} ->
      vote(NumVote, NumSorry-1, Round)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) ->
    send(Acceptor, {prepare, self(), Round})
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) ->
    send(Acceptor, {accept, self(), Round, Proposal})
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  if is_tuple(Name) -> %remote
    Name ! Message;
  true -> %local
    case whereis(Name) of
      undefined ->
        down;
      Pid ->
        Pid ! Message
      end
    end.
