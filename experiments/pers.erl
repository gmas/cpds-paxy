-module(pers).
-export([open/1, read/1, store/5, close/1, delete/1]).

%% dets module provides term storage on file

open(Name) ->
    FileName = io_lib:format("~s.pers", [Name]),
    dets:open_file(FileName, []).

%% returns the object with the key 'perm' stored in the table 'Name'
read(Name) ->
    FileName = io_lib:format("~s.pers", [Name]),
    case dets:lookup(FileName, perm) of
        [{perm, Pr, Vt, Ac, Pn}] ->
            {Pr, Vt, Ac, Pn};
        [] ->
            {order:null(), order:null(), na, na}
    end.

%% inserts one object {Pr, Vt, Ac, Pn} into the table 'Name'
store(Name, Pr, Vt, Ac, Pn)->
    FileName = io_lib:format("~s.pers", [Name]),
    dets:insert(FileName, {perm, Pr, Vt, Ac, Pn}).

close(Name) ->
    FileName = io_lib:format("~s.pers", [Name]),
    dets:close(FileName).

delete(Name) ->
    FileName = io_lib:format("~s.pers", [Name]),
    file:delete(FileName).
