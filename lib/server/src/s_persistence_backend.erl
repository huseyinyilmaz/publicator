-module(s_persistence_backend).

-callback init(Args :: list(term())) -> 'ok'|tuple('error', Reason :: string()).

-callback handle(Event :: atom()) -> NextEvent :: atom().

-callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 'ok'|tuple('error', Reason :: string()).


-callback insertMessage(Channel :: binary(), Msg)-> ok.
