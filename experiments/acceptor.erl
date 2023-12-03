-module(acceptor).
-export([start/2]).

-define(delay, 0).
-define(drop_prob, 0).
-define(drop_sorry, false).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  Promised = order:null(),
  Voted = order:null(),
  Value = na,
  pers:open(Name),
  {Pr, Vt, Ac, Pn} = pers:read(Name),
  case Pn == na of
    true ->
      acceptor(Name, Promised, Voted, Value, PanelId);
    false ->
      Pn ! {updateAcc, "Restarted: " ++ io_lib:format("~p", [Vt]),
          "Promised: " ++ io_lib:format("~p", [Pr]), Ac},
      acceptor(Name, Pr, Vt, Ac, Pn)
    end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          pers:store(Name, Round, Voted, Value, PanelId),
          send(Name, Proposer, {promise, Round, Voted, Value}, ?delay, ?drop_prob, ?drop_sorry),
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          send(Name, Proposer, {sorry, {prepare, Round}}, ?delay, ?drop_prob, ?drop_sorry),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          send(Name, Proposer, {vote, Round}, ?delay, ?drop_prob, ?drop_sorry),
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              pers:store(Name, Promised, Round, Value, PanelId),
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          send(Name, Proposer, {sorry, {accept, Round}}, ?delay, ?drop_prob, ?drop_sorry),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.

send(Name, Proposer, Message, Delay_num) ->
  if Delay_num > 0 ->
      T = rand:uniform(Delay_num),
      timer:send_after(T, Proposer, Message),
      io:format("[Acceptor ~w] Send MSG: ~w to ~w with delay ~w ms~n", [Name, Message, Proposer, T]);
  true ->
      Proposer ! Message
  end.

send(Name, Proposer, Message, Delay_num, Drop_prob) ->
  P = rand:uniform(100),
  if P =< Drop_prob ->
    io:format("[Acceptor ~w] Drop MSG: ~w to ~w~n", [Name, Message, Proposer]);
  true ->
    send(Name, Proposer, Message, Delay_num)
  end.

send(Name, Proposer, Message, Delay_num, Drop_prob, Drop_sorry) ->
  if Drop_sorry ->
    if element(1, Message) == sorry ->
      io:format("[Acceptor ~w] Drop sorry: ~w to ~w~n", [Name, Message, Proposer]);
    true ->
      send(Name, Proposer, Message, Delay_num, Drop_prob)
    end;
  true ->
      send(Name, Proposer, Message, Delay_num, Drop_prob)
  end.