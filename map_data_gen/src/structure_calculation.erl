-module(structure_calculation).
-behaviour(gen_server).
-define(SERVER,?MODULE).

% API commands
-export([start/0,start/1,start/2,start/3,stop/0,decide_struct/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API calls
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------

-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------

-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, [], []).

-spec start(atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------

-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).


%%--------------------------------------------------------------------
%% @doc
%% decide_struct call
%% decides if a square should be a structure based on surounding squares.
%%
%% @end
%%--------------------------------------------------------------------
-spec decide_struct(term(),tuple()) -> term().
decide_struct(Name,Squares) -> gen_server:call(Name,Squares).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,running}.





%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
% handle_call(_Request, _From, State) ->
%         {reply,replace_started,State};
handle_call(stop, _From, _State) ->
        {stop,normal,
                stopped,
          down}; %% setting the server's internal state to down
% {{_,_},none}

% No left or top squares.
handle_call({{{_,_},none}, {{_,_},none}, {{Positive_x,Zero_y},{value,PZ_square}}, 
            {{Positive_x,Negative_y},{value,PN_square}}, {{Zero_x,Negative_y},{value,ZN_square}}, {{_,_},none}, 
            {{_,_},none}, {{_,_},none},Next_struct},_From,State) ->

        Squares = [PZ_square,PN_square,ZN_square],

        Struct_value = calculate_structure(Squares,Next_struct),
    {reply,
        Struct_value,
        State};

% No top or right squares
handle_call({{{_,_},none}, {{_,_},none}, {{_,_},none}, {{_,_},none}, {{Zero_x,Negative_y},{value,ZN_square}}, 
            {{Negative_x,Negative_y},{value,NN_square}}, {{Negative_x,Zero_y},{value,NZ_square}}, {{_,_},none},
            Next_struct},_From,State) ->

    Squares = [ZN_square,NN_square,NZ_square],

    Struct_value = calculate_structure(Squares,Next_struct),

    {reply,
        Struct_value,
        State};

% No right squares
handle_call({{{_,_},none}, {{_,_},none}, 
            {{Positive_x,Zero_y},{value,PZ_square}}, {{Positive_x,Negative_y},{value,PN_square}}, 
            {{Zero_x,Negative_y},{value,ZN_square}}, {{Negative_x,Negative_y},{value,NN_square}}, 
            {{Negative_x,Zero_y},{value,NZ_square}}, {{_,_},none},Next_struct},_From,State) ->

    Squares = [PZ_square,PN_square,ZN_square,NN_square,NZ_square],

    Struct_value = calculate_structure(Squares,Next_struct),

    {reply,
        Struct_value,
        State};


handle_call({{{Zero_x,Positive_y},{value,ZP_square}}, {{_,_},none}, {{_,_},none}, {{_,_},none}, 
            {{Zero_x,Negative_y},{value,ZN_square}}, {{Negative_x,Negative_y},{value,NN_square}}, 
            {{Negative_x,Zero_y},{value,NZ_square}}, {{Negative_x,Positive_y},{value,NP_square}},Next_struct},_From,State) ->

    Squares = [ZP_square,ZN_square,NN_square,NZ_square,NP_square],

    Struct_value = calculate_structure(Squares,Next_struct),

    {reply,
        Struct_value,
        State};

handle_call({{{Zero_x,Positive_y},{value,ZP_square}}, {{_,_},none}, {{_,_},none}, {{_,_},none}, {{_,_},none}, 
            {{_,_},none}, {{Negative_x,Zero_y},{value,NZ_square}}, {{Negative_x,Positive_y},{value,NP_square}},Next_struct},_From,State) ->

    Squares = [ZP_square,NZ_square,NP_square],

    Struct_value = calculate_structure(Squares,Next_struct),

    {reply,
        Struct_value,
        State};

handle_call({{Square_x,Square_y}, {{Zero_x,Positive_y},{value,ZP_square}}, {{Positive_x,Positive_y},{value,PP_square}}, 
            {{Positive_x,Zero_y},{value,PZ_square}}, {{Positive_x,Negative_y},{value,PN_square}}, 
            {{Zero_x,Negative_y},{value,ZN_square}}, {{Negative_x,Negative_y},{value,NN_square}}, 
            {{Negative_x,Zero_y},{value,NZ_square}}, {{Negative_x,Positive_y},{value,NP_square}},Next_struct},_From,State) ->

    Squares = [ZP_square,PP_square,PZ_square,PN_square,ZN_square,NN_square,NZ_square,NP_square],

    Struct_value = calculate_structure(Squares,Next_struct),

    {reply,
        Struct_value,
        State}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% Determines if a structure will be added, and if one will, which one.
%%
%% @end
%%--------------------------------------------------------------------

which_structure([],Held_struct) -> Held_struct;
which_structure([H|_],Held_struct) when 0 < Held_struct, Held_struct /= H -> 0;
which_structure([H|T],Held_struct) when 0 < H, Held_struct == 0 -> which_structure(T,H);
which_structure([H|T],Held_struct) when 0 < Held_struct, Held_struct == H -> which_structure(T,Held_struct);
which_structure([0|T],Held_struct) -> which_structure(T,Held_struct).




calculate_structure(Squares,Next_struct) ->
    Struct_value = which_structure(Squares,0),
    case Struct_value of
        0 -> Struct_choices = construct_struct_choices([0],Next_struct);
        _Else -> Struct_choices = construct_struct_choices(Squares,Struct_value),
                lists:nth(rand:uniform(length(Struct_choices)),Struct_choices)
    end.


construct_struct_choices(Squares,Struct_value) when length(Squares) == 9 -> Squares;
construct_struct_choices(Squares,Struct_value) when length(Squares) < 5 -> construct_struct_choices([Struct_value|Squares],Struct_value);
construct_struct_choices(Squares,Struct_value) when length(Squares) < 9 -> construct_struct_choices([0|Squares],Struct_value).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.