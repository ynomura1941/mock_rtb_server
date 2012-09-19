%% -*- coding: utf-8 -*-
-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~s = ~p~n", [?MODULE,?LINE,??X,X])).
-else.
-define(LOG(X), ok).
-endif.

