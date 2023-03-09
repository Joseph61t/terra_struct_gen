-module(diamond_square).
-behaviour(gen_server).
-define(SERVER,?MODULE).

% API commands
-export([start/0,start/1,start/2,start/3,stop/0,diamond_step/3,square_step/3]).

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
%% Diamond_step call
%% Get a square's center varied. 
%% Called diamond step because it gets the center of the diamond
%%
%% @end
%%--------------------------------------------------------------------
-spec diamond_step(term(),list(),tuple()) -> term().
diamond_step(Name,Vary_list,Square) -> gen_server:call(Name,{diamond,Vary_list,Square}).


%%--------------------------------------------------------------------
%% @doc
%% Diamond_step call
%% Get a square's center varied. 
%% Called diamond step because it gets the center of the diamond
%%
%% @end
%%--------------------------------------------------------------------
-spec square_step(term(),list(),tuple()) -> term().
square_step(Name,Vary_list,Square) -> gen_server:call(Name,{square,Vary_list,Square}).


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

% Any x or y does not exist
handle_call({diamond, _, {{{_,_},none}, _, _, _}},_From,State) ->
    {reply,
        none,
        State};
handle_call({diamond, _, {_, {{_,_},none}, _, _}},_From,State) ->
    {reply,
        none,
        State};
handle_call({diamond, _, {_, _, {{_,_},none}, _, _}},_From,State) ->
    {reply,
        none,
        State};
handle_call({diamond, _, {_, _, _, {{_,_},none}}},_From,State) ->
    {reply,
        none,
        State};

% All xs and ys exist
handle_call({diamond, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, 
            {{X_left,Y_bottom}, {value,Height_lb}}, {{X_right,Y_bottom}, {value,Height_rb}}}},_From,State) ->
    {reply,
        {{(X_left + (X_right - X_left) div 2),
        (Y_top + (Y_bottom - Y_top) div 2)},
        calculate_height([Height_lb, Height_lt, Height_rb, Height_rt],Vary_list)},
        State};


% Far right x does not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% Far left x does not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% Very top y does not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, 
            {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% Very bottom y does not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{_,_},none}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb],Vary_list)}], % bottom
        State};

% Far left x and very top y do not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, 
            {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% Far right x and very top y do not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, 
            {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% Far right x and very bottom y do not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{_,_},none}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb],Vary_list)}], % bottom
        State};

% Far left x and very bottom y do not exist
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{_,_},none}, {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb],Vary_list)}], % bottom
        State};

% all Far xs and very top and bottom ys don't exist.
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, 
    {value,Height_lb}}, {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{_,_},none}, {{_,_},none},
    {{_,_},none}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb],Vary_list)}], % bottom
        State};

% No left x
handle_call({square, Vary_list, {{{_,_},none}, {{X_right,Y_top}, {value,Height_rt}}, {{_,_},none}, {{X_right,Y_bottom}, {value,Height_rb}},
            {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, 
            {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            none, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% No right x
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{_,_},none}, {{X_left,Y_bottom}, {value,Height_lb}}, {{_,_},none},
            {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, 
            {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_mvt],Vary_list)}, % top
            none, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_mvb],Vary_list)}], % bottom
        State};

% No top y
handle_call({square, Vary_list, {{{_,_},none}, {{_,_},none}, {{X_left,Y_bottom}, {value,Height_lb}}, {{X_right,Y_bottom}, {value,Height_rb}},
            {{Mid_x,Mid_y},{value,Height_m}}, {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, 
            {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lb,Height_mfl],Vary_list)}, % left
            none, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State};

% No bottom y
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{_,_},none}, {{_,_},none}, 
            {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, {{_,_},none}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, 
            {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_mfr],Vary_list)}, % right
            none], % bottom
        State};

% THIS IS A CORNER AND DOES NOT, AND SHOULD NOT GET CALLED.
% No left x and no top y
handle_call({square, Vary_list, {{{_,_},none}, {{_,_},none}, {{_,_},none}, {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, 
            {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            none, % left
            none, % top
            none, % right
            none], % bottom
        State};

% THIS IS A CORNER AND DOES NOT, AND SHOULD NOT GET CALLED.
% No right x and no top y
handle_call({square, Vary_list, {{{_,_},none}, {{_,_},none}, {{X_left,Y_bottom}, {value,Height_lb}}, {{_,_},none}, {{Mid_x,Mid_y},{value,Height_m}}, 
            {{_,_},none}, {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            none, % left
            none, % top
            none, % right
            none], % bottom
        State};

% THIS IS A CORNER AND DOES NOT, AND SHOULD NOT GET CALLED.
% No left x and no bottom y
handle_call({square, Vary_list, {{{_,_},none}, {{X_right,Y_top}, {value,Height_rt}}, {{_,_},none}, {{_,_},none}, {{Mid_x,Mid_y},{value,Height_m}}, 
            {{Mid_x,_Very_top_y},{value,Height_mvt}}, {{_,_},none}, {{_,_},none}, {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            none, % left
            none, % top
            none, % right
            none], % bottom
        State};

% THIS IS A CORNER AND DOES NOT, AND SHOULD NOT GET CALLED.
% No right x and no bottom y. 
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{_,_},none}, {{_,_},none}, {{_,_},none}, {{Mid_x,Mid_y},{value,Height_m}}, 
            {{Mid_x,_Very_top_y},{value,Height_mvt}}, {{_,_},none}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, {{_,_},none}}},_From,State) ->
    {reply,
        [
            none, % left
            none, % top
            none, % right
            none], % bottom
        State};

% All xs and ys exist.
handle_call({square, Vary_list, {{{X_left,Y_top}, {value,Height_lt}}, {{X_right,Y_top}, {value,Height_rt}}, {{X_left,Y_bottom}, {value,Height_lb}}, 
            {{X_right,Y_bottom}, {value,Height_rb}}, {{Mid_x,Mid_y},{value,Height_m}}, {{Mid_x,_Very_top_y},{value,Height_mvt}}, 
            {{Mid_x,_Very_bottom_y},{value,Height_mvb}}, {{_Mid_Far_left_x,Mid_y},{value,Height_mfl}}, 
            {{_Mid_Far_right_x,Mid_y},{value,Height_mfr}}}},_From,State) ->
    {reply,
        [
            {{X_left, Mid_y}, calculate_height([Height_m,Height_lt,Height_lb,Height_mfl],Vary_list)}, % left
            {{Mid_x, Y_top}, calculate_height([Height_m,Height_lt,Height_rt,Height_mvt],Vary_list)}, % top
            {{X_right, Mid_y}, calculate_height([Height_m,Height_rt,Height_rb,Height_mfr],Vary_list)}, % right
            {{Mid_x, Y_bottom}, calculate_height([Height_m,Height_lb,Height_rb,Height_mvb],Vary_list)}], % bottom
        State}.




%%--------------------------------------------------------------------
%% @doc
%%
%% Calculates the average for the heights passed to it, and returns that
%% plus a random value from the vary_list
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_height(list(),list()) -> float().
calculate_height(Nodes,Vary_list) -> 
    lists:sum(Nodes)/length(Nodes) + lists:nth(rand:uniform(length(Vary_list)),Vary_list).
    % lists:sum(Nodes)/length(Nodes).
    % lists:sum(Nodes)/4 + lists:nth(rand:uniform(length(Vary_list)),Vary_list).


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