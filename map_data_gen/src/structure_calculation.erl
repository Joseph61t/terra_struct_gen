-module(structure_calculation).
-behaviour(gen_server).
-define(SERVER,?MODULE).

% API commands
-export([start/0,start/1,start/2,start/3,stop/0,decide_struct/5]).

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
-spec decide_struct(term(),term(),tuple(),term(),tuple()) -> term().
decide_struct(Name,Square_corners,Size,Structures,Struct) -> gen_server:call(Name,{Square_corners,Size,Structures,Struct}).

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


handle_call({{{{X1,Y1},{value,Value_11}},{{X1,Y2},{value,Value_12}},{{X2,Y1},{value,Value_21}},{{X2,Y2},{value,Value_22}}},
            Size,Structures,Next_struct},_From,State) ->

    % Squares = [PZ_square,PN_square,ZN_square],
    Square = [make_tuple_value(Value_11),make_tuple_value(Value_12),make_tuple_value(Value_21),make_tuple_value(Value_22)],
    case can_be_struct(Square) of
        true -> Structure_list = calculate_structure(Square,Structures,Next_struct),
                Struct_value = lists:nth(rand:uniform(length(Structure_list)),Structure_list);
        _Else -> Struct_value = 0
    end,
    case Struct_value of
        0 -> io:format("not struct: ~n~p | ~p~n~p | ~p~n",[{{X1,Y1},get_struct(Value_11)},{{X1,Y2},get_struct(Value_12)},{{X2,Y1},get_struct(Value_21)},{{X2,Y2},get_struct(Value_22)}]);
        _ELse -> io:format("is struct:  ~n~p | ~p~n~p | ~p~n",[{{X1,Y1},Struct_value},{{X1,Y2},Struct_value},{{X2,Y1},Struct_value},{{X2,Y2},Struct_value}])
    end,
    {reply,
        Struct_value,
        State};

handle_call({Square,_Size,_Structures,_Next_struct},_From,State) -> 
    % io:format("~p~n",[Square]),
    {reply,
        0,
        State}.


%%--------------------------------------------------------------------
%% @doc
%% 
%% Determines if a structure will be added, and if one will, which one.
%%
%% @end
%%--------------------------------------------------------------------

can_be_struct([{H1,_},{H2,_},{H3,_},{H4,_}]) when abs(abs(H1)-abs(H2)) < 1, abs(abs(H2)-abs(H3)) < 1, 
                                                    abs(abs(H3)-abs(H4)) < 1, abs(abs(H4)-abs(H1)) < 1 -> true;
can_be_struct(_) -> false.

calculate_structure(Square,Structures,Next_struct) ->
    Structs = [Struct || {_,Struct} <- Square],
    % Struct_value = which_structure(Structs,0),
    case which_structure(Structs,0) of
        none -> Struct_choices = [0];
        0 -> Struct_choices = [0,Next_struct];
        Else -> case dict:fetch(integer_to_list(Else),Structures) of
                    done -> Struct_choices = [0];
                    _Else -> Struct_value = Else,
                            case round(lists:sum(Structs) / Struct_value) of
                                0 -> Struct_choices = [Next_struct,0];
                                1 -> Struct_choices = [Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,0,0,0,0];
                                2 -> Struct_choices = [Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,0,0,0];
                                3 -> Struct_choices = [Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,Struct_value,0,0];
                                4 -> Struct_choices = [Struct_value]
                            end
                end
    end,
    Struct_choices.

which_structure([],Held_struct) -> Held_struct;
which_structure([0|T],Held_struct) -> which_structure(T,Held_struct);
which_structure([H|T],Held_struct) when 0 < H, Held_struct == 0 -> which_structure(T,H);
which_structure([H|T],Held_struct) when 0 < Held_struct, Held_struct == H -> which_structure(T,Held_struct);
which_structure([H|_],Held_struct) when 0 < Held_struct, Held_struct /= H -> none.


get_height({Height,_}) -> Height;
get_height(Height) -> Height.

get_struct({_,Struct}) -> Struct;
get_struct(_) -> 0.

make_tuple_value(Value) -> 
    % io:format("Value: ~p~n",[Value]),
    {get_height(Value),get_struct(Value)}.

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