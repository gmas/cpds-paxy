-module(acceptor).
-export([start/2]).

-define(delay, 500).
-define(drop, 1).
-define(drop_chance, 10).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  Promised = order:null(),
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          send(Name, Proposer, {promise, Round, Voted, Value}, ?delay, ?drop_chance),
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          %send(Name, Proposer, {sorry, {prepare, Round}}, ?delay, ?drop_chance),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          send(Name, Proposer, {vote, Round}, ?delay, ?drop_chance),
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          %send(Name, Proposer, {sorry, {accept, Round}}, ?delay, ?drop_chance),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.

send(Name, Proposer, Message, Delay_num, Drop_Chance) ->
  P = rand:uniform(Drop_Chance),
  if P =< ?drop ->
    io:format("[Acceptor ~w] Drop MSG: ~w to ~w~n", [Name, Message, Proposer]);
  true ->
    if Delay_num > 1 ->
        T = rand:uniform(Delay_num),
        timer:send_after(T, Proposer, Message),
        io:format("[Acceptor ~w] Send MSG: ~w to ~w with delay ~w~n", [Name, Message, Proposer, T]);
      true ->
        Proposer ! Message
    end
  end.
