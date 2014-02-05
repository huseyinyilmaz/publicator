%% Feel free to use, reuse and abuse the code in this file.

%% @doc EventSource emitter.
-module(h_eventsource_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(state, {session_id::binary()}).

init(_Transport, Req, []) ->
        Headers = [{<<"content-type">>, <<"text/event-stream">>}],
        {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
        {Session_id, Req3} = cowboy_req:binding(session, Req2),
        State = #state{session_id=Session_id},
        {loop, Req3, State}.

info({message, Msg}, Req, State) ->
        ok = cowboy_req:chunk(["id: ", id(), "\ndata: ", Msg, "\n\n"], Req),
        erlang:send_after(1000, self(), {message, "Tick"}),
        {loop, Req, State}.

terminate(_Reason, _Req, #state{session_id=Session_id}) ->
    lager:debug("Terminate eventsource handler for session ~p", [Session_id]),
    %% XXX do not stop consumer?
    ok = server:stop_consumer(Session_id),
    ok.

id() ->
        {Mega, Sec, Micro} = erlang:now(),
        Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
        integer_to_list(Id, 16).
