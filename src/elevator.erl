-module(elevator).
-behavior(gen_server).

-export([start_link/1, call/3, goto/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-define(UP, up).
-define(DOWN, down).

-record(state, {
         direction = null,
         queue = [],
         floor = 0,
         listeners = [],
         nfloors}).

% Client API
start_link(Floors) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Floors], []),
    F = fun() -> run(Pid) end,
    spawn(F), {ok, Pid}.

call(Pid, Floor, Direction) ->
    gen_server:call(Pid, {call_elevator, Floor, Direction}).

goto(Pid, Floor) ->
    gen_server:cast(Pid, {goto_floor, Floor}).

run(Pid) ->
    receive
    after 1000 ->
              gen_server:call(Pid, {process_elevator}),
              run(Pid)
    end.

% Server methods
init([Floors]) ->
    {ok, #state{nfloors=Floors}}.

handle_call({status}, _, State) ->
    io:format("Current status is ~p ~n", State#state.floor),
    {reply, State, State};

handle_call({call_elevator, Floor, Direction}, From, #state{queue=Queue, direction=CurrDirection, listeners=Listeners} = State) ->
    Itm = {Direction, Floor},
    S=case lists:member(Itm, Queue) of
        false -> State#state{queue=[Itm|Queue]};
        _  -> State
    end,
    Itm2 = {Floor, From},
    S2=case lists:member(Itm2, Listeners) of
        false -> S#state{listeners=[Itm2|Listeners]};
        _     -> S
    end,
    case CurrDirection of
        null -> {noreply, S2#state{direction=Direction}};
        _    -> {noreply, S2}
    end;

handle_call({process_elevator}, _From, #state{queue=Q, direction=D} = State) ->
    io:format("Processing elevator ~n"),
    LessFn = fun({Dir1, Floor1}, {Dir2, Floor2}) -> 
                  case Dir1 =:= Dir2 of
                      true ->
                          case D =:= down of
                              true  -> Floor1 < Floor2;
                              false -> Floor1 > Floor2
                          end;
                     false ->
                          case Dir1 =:= up of
                              true -> D == up;
                              false -> D == down
                          end
                 end
            end,
    SortedQ = lists:sort(LessFn, Q),
    Len = length(SortedQ),
    [I|Q2] = SortedQ,
    {NextDirection, NextFloor} = I,
    Reply = case Len > 1 of
        true ->
            {reply, done, State#state{direction=NextDirection, floor=NextFloor, queue=Q2}};
        false ->
            case Len == 1 of
               true ->
                    {reply, done, State#state{direction=null, queue=[]}};
               false ->
                    {reply, done, State}
            end
    end,
    io:format("Moving to floor ~p ~n", [NextFloor]),
    Reply.

handle_cast({goto_floor, Floor}, #state{floor=Floor} = State) ->
    {noreply, State};

handle_cast({goto_floor, Floor}, #state{queue=Queue, floor=CurrFloor} = State) ->
    Direction=case CurrFloor < Floor of
        true -> up;
        _    -> down
    end,
    Itm = {Direction, Floor},
    S=case lists:member(Itm, Queue) of
        false -> State#state{queue=[Itm|Queue]};
        _  -> State
    end,
    {noreply, S}.

handle_info(_, State) ->
    io:format("Invalid message : ~p ~n", [State]),
    {noreply, State}.

terminate(normal, State) ->
    io:format("Stopping elevator, state : ~p ~n", [State]),
    ok.

code_change(_, State, _) ->
    {ok, State}.

