-module(acceptor).
-export([start/2]).

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
      case order:gr({Round, Proposer}, {Promised}) of
        true ->
          Proposer ! {promise, Round, Voted, Value},
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, {Round, Proposer}, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [{Round, Proposer}]), Colour},
          acceptor(Name, {Round, Proposer}, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe({Round, Proposer}, Promised) of
        true ->
          Proposer ! {vote, {Round, Proposer}},
          % check if vote round > max Voted round
          case order:goe({Round, Proposer}, Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, {Round, Proposer}, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [{Round, Proposer}]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, {Round, Proposer}, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
