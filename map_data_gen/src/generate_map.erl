-module(generate_map).
-behaviour(gen_server).
-define(SERVER,?MODULE).

-export([start/1,start/2,start/3,stop/0,random_map/1,chosen_map/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).



-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
    main_frame:start(),
    {ok,running}.

-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, [], []).

-spec start(atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(gen_maps, stop).


random_map(Size_factor) -> gen_server:call(gen_map,{random_map,Size_factor}).

chosen_map(Map_options) -> gen_server:call({chosen_map,Map_options}).


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
handle_call({random_map,Size_factor}, _From, _State) ->
    Size = round(math:pow(2,Size_factor)),
    % Ex_mountains = 7,
    % Mountains = 5,
    % Plains = 3,
    % Ex_plains = 1,
    % Intensity = lists:nth(rand:uniform(3),[Ex_mountains,Mountains,Plains,Ex_plains]),

    Intensity = lists:nth(rand:uniform(3),[7,5,3,1]),

    Vary_reduce = 10 - Size_factor,
    Variance = Size * 2 * Intensity,
    io:format("Intensity: ~p~n",[Intensity]),
    io:format("Vary_reduce: ~p~n",[Vary_reduce]),
    io:format("Starting Variance: ~p~n",[Variance]),

    Low_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Intensity-Variance,Variance+Intensity)),
    High_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Size+Intensity-Variance,Size+Intensity+Variance)),
    Low_balanced = [Intensity],
    % Low_plains = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),
    High_balanced = [Size+Intensity],
    % High_plains = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),

    % io:format("||~p||",[Vary_list]),
    % Terrain_type = lists:nth(4,[Low_unbalanced,High_unbalanced,Low_balanced,High_balanced]),
    
    case lists:nth(rand:uniform(4),[0,1,2,3]) of
        0 -> io:format("Low_unbalanced~n"),
            Terrain_type = Low_unbalanced;
        1 -> io:format("High_unbalanced~n"),
            Terrain_type = High_unbalanced;
        2 -> io:format("Low_balanced~n"),
            Terrain_type = Low_balanced;
        3 -> io:format("High_balanced~n"),
            Terrain_type = High_balanced
    end,

    Results = make_map(Size_factor,Intensity,Terrain_type),

    {reply, Results, down};

handle_call({chosen_map,{Size_factor,Intensity,Height,Original_stability}}, _From, _State) ->
    Size = round(math:pow(2,Size_factor)),
    Variance = Size * 2 * Intensity,

    Low_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Intensity-Variance,Variance+Intensity)),
    High_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Size+Intensity-Variance,Size+Intensity+Variance)),
    Low_balanced = [Intensity],
    High_balanced = [Size+Intensity],

    case Height of
        0 -> case Original_stability of
                0 -> Terrain_type = Low_balanced;
                1 -> Terrain_type = Low_unbalanced
            end;
        1 -> case Original_stability of
                0 -> Terrain_type = High_balanced;
                0 -> Terrain_type = High_unbalanced
            end
    end,

    Results = make_map(Size_factor,Intensity,Terrain_type),
    {reply,Results,_State};

handle_call(stop, _From, _State) ->
        {stop,normal,
                stopped,
          down}. %% setting the server's internal state to down

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
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



make_map(Size_factor,Intensity,Terrain_type) ->
    Size = round(math:pow(2,Size_factor)),
    Step = Size,
    Vary_reduce = 10 - Size_factor,
    Variance = Size * 2 * Intensity,
% -spec make_map(term()) -> term().
% make_map(Size_factor) ->
%     Size = round(math:pow(2,Size_factor)),
%     % Variance = Size*10,
%     Step = Size,
%     % Step = round(Size/2),
%     % Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),

%     Ex_mountains = 7,
%     Mountains = 5,
%     Plains = 3,
%     Ex_plains = 1,

%     Intensity = lists:nth(rand:uniform(3),[Ex_mountains,Mountains,Plains,Ex_plains]),
%     % Intensity = 1,
%     io:format("Intensity: ~p~n",[Intensity]),
%     Vary_reduce = 9 - Size_factor,
%     io:format("Vary_reduce: ~p~n",[Vary_reduce]),
%     Variance = Size * 2 * Intensity,
%     io:format("Starting Variance: ~p~n",[Variance]),

%     Low_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Intensity-Variance,Variance+Intensity)),
%     High_unbalanced = lists:map(fun(X) -> X/10 end, lists:seq(Size+Intensity-Variance,Size+Intensity+Variance)),
%     Low_balanced = [Intensity],
%     % Low_plains = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),
%     High_balanced = [Size+Intensity],
%     % High_plains = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),

%     % io:format("||~p||",[Vary_list]),
%     % Terrain_type = lists:nth(4,[Low_unbalanced,High_unbalanced,Low_balanced,High_balanced]),
    
%     case lists:nth(rand:uniform(4),[0,1,2,3]) of
%         0 -> io:format("Low_unbalanced~n"),
%             Terrain_type = Low_unbalanced;
%         1 -> io:format("High_unbalanced~n"),
%             Terrain_type = High_unbalanced;
%         2 -> io:format("Low_balanced~n"),
%             Terrain_type = Low_balanced;
%         3 -> io:format("High_balanced~n"),
%             Terrain_type = High_balanced
%     end,
    
    Terrain_map1 = gb_trees:from_orddict(orddict:from_list([{{X,Y},{0,0}} || X <- lists:seq(0,Size), Y <- lists:seq(0,Size)])),
    % io:format("terrain_map before updates: ~p~n",[Terrain_map1]),
    Terrain_map2 = gb_trees:update({0,0},{lists:nth(rand:uniform(length(Terrain_type)),Terrain_type),0},Terrain_map1),
    Terrain_map3 = gb_trees:update({0,Size},{lists:nth(rand:uniform(length(Terrain_type)),Terrain_type),0},Terrain_map2),
    Terrain_map4 = gb_trees:update({Size,0},{lists:nth(rand:uniform(length(Terrain_type)),Terrain_type),0},Terrain_map3),
    Terrain_map5 = gb_trees:update({Size,Size},{lists:nth(rand:uniform(length(Terrain_type)),Terrain_type),0},Terrain_map4),
    % io:format("terrain_map after creation: ~p~n",[Terrain_map5]),
    
    Finished_terrain_map = vary_map(Step,
                    Size,
                    Vary_reduce,
                    Variance,
                    Terrain_map5),
    Structured_map = add_structures(Size*Size,Size,{0,0,1,1},dict:new(),Finished_terrain_map,1),
    Map = gb_trees:to_list(Structured_map),
    % io:format("~p~n",[Structured_map2]),
    % Map = gb_trees:to_list(Finished_terrain_map),
    Map_name = save_map(1,
                        string:concat("/home/orindale/Programing/terra_struct_gen/map_creation/maps_datas/Size_", 
                                      integer_to_list(Size)),
                        Map),
    % Run python file
    Python_to_use = "/usr/bin/python ",
    Python_program_path = "/home/orindale/Programing/terra_struct_gen/map_creation/make_map.py ",
    Command = string:concat(string:concat(Python_to_use,Python_program_path),
                            string:concat(string:concat("--map_size ",integer_to_list(Size)),
                                          string:concat(" --map_path ", Map_name))),
    os:cmd(Command).
    % Vary_list. 

save_map(_Count,Name,Terrain_map) ->
    % file:write_file(Name, Terrain_map).
    case file:read_file_info(Name) of
        {error, enoent} -> file:write_file(Name, io_lib:fwrite("~p.\n",[Terrain_map])),
                           Name;
        _Else -> file:delete(Name),
                 file:write_file(Name, io_lib:fwrite("~p.\n",[Terrain_map])),
                 Name
    end.

-spec add_structures(term(),term(),term(),term(),term(),term()) -> term().
add_structures(1,Size,{X1,Y1,X2,Y2},Structures,Map,Struct) ->
    Square_corners = {
        {{round(X1),round(Y1)},gb_trees:lookup({X1,Y1},Map)},
        {{round(X1),round(Y2)},gb_trees:lookup({X1,Y2},Map)},
        {{round(X2),round(Y1)},gb_trees:lookup({X2,Y1},Map)},
        {{round(X2),round(Y2)},gb_trees:lookup({X2,Y2},Map)}
    },
    % Points = [{X,Y} || X <- [X1,X2], Y <- [Y1,Y2]],
    % Heights = [gb_trees:lookup(Point,Map)|| Point <- Points],
    Structure = structure_calculation:decide_struct(calc_struct,Square_corners,Size+1,Structures,Struct),
    case Structure of
        false -> Map;
        _Else -> update_structure_map([{X1,Y1},{X1,Y2},{X2,Y1},{X2,Y2}],{{X1,Y1},{X1,Y2},{X2,Y1},{X2,Y2}},Structure, Map)
    end;

add_structures(Squares_left,Size,{X1,Y1,X2,Y2},Structures,Map,Struct) ->

    Square_corners = {
        {{round(X1),round(Y1)},gb_trees:lookup({X1,Y1},Map)},
        {{round(X1),round(Y2)},gb_trees:lookup({X1,Y2},Map)},
        {{round(X2),round(Y1)},gb_trees:lookup({X2,Y1},Map)},
        {{round(X2),round(Y2)},gb_trees:lookup({X2,Y2},Map)}
    },
    % Points = [{X,Y} || X <- [X1,X2], Y <- [Y1,Y2]],
    % Heights = [gb_trees:lookup(Point,Map)|| Point <- Points],
    Structure = structure_calculation:decide_struct(calc_struct,Square_corners,Size+1,Structures,Struct), %%%% START HERE
    case Structure of
        0 -> Structure_map = Map,
             New_structures = Structures;
        _Else -> case dict:is_key(integer_to_list(Structure),Structures) of
                    true -> New_structures = dict:store(integer_to_list(Structure),lists:nth(2,[dict:fetch(integer_to_list(Structure),Structures)+1,done]),Structures);
                    _ELSE -> New_structures = dict:store(integer_to_list(Structure),1,Structures)
                end,
                Structure_map = update_structure_map([{X1,Y1},{X1,Y2},{X2,Y1},{X2,Y2}],{{X1,Y1},{X1,Y2},{X2,Y1},{X2,Y2}}, Structure, Map)
    end,
    case Y1 == Size of
        true -> New_X1 = X1 + 1,
                New_X2 = X2 + 1,
                New_Y1 = 0,
                New_Y2 = 1;
        _ELse -> New_X1 = X1,
                 New_X2 = X2,
                 New_Y1 = Y1 + 1,
                 New_Y2 = Y2 + 1
    end,
    case Structure == Struct of
        true -> Next_struct = Struct + 1;
        _ELSe -> Next_struct = Struct
    end,
    add_structures(Squares_left-1,Size,{New_X1,New_Y1,New_X2,New_Y2},New_structures,Structure_map,Next_struct).

-spec vary_map(term(),term(),term(),term(),term()) -> term().
vary_map(1,Size,Vary_reduce,Variance,Terrain_map) -> 
    % io:format("Step of 1"),
    % io:format("Variance: ~p~n",[Variance]),
    % io:format("terrain_map: ~p~n",[Terrain_map]),

    % Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor((0-Variance)*10),floor(Variance*10))),
    Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor(0-Variance),floor(Variance))),
    % Vary_list = [0.0],
    % io:format("~p~n",[Vary_list]),

    Squares = [{
                    {{X,Y},gb_trees:lookup({X,Y},Terrain_map)},
                    {{X+1,Y},gb_trees:lookup({X+1,Y},Terrain_map)},
                    {{X,Y+1},gb_trees:lookup({X,Y+1},Terrain_map)},
                    {{X+1,Y+1},gb_trees:lookup({X+1,Y+1},Terrain_map)}
                } 
                || X <- lists:seq(0,Size-1,1), Y <- lists:seq(0,Size-1,1)],

    Diamond_mids = [diamond_square:diamond_step(disq,Vary_list,Square) || Square <- Squares],
    Terrain_map2 = update_map(Diamond_mids,Terrain_map),
    Diamonds = [{
                    {{round(X-1/2),round(Y-1/2)},gb_trees:lookup({round(X-1/2),round(Y-1/2)},Terrain_map2)}, %
                    {{round(X+1/2),round(Y-1/2)},gb_trees:lookup({round(X+1/2),round(Y-1/2)},Terrain_map2)}, %
                    {{round(X-1/2),round(Y+1/2)},gb_trees:lookup({round(X-1/2),round(Y+1/2)},Terrain_map2)}, %
                    {{round(X+1/2),round(Y+1/2)},gb_trees:lookup({round(X+1/2),round(Y+1/2)},Terrain_map2)}, %
                    {{X,Y},{value,{Mid_height,0}}}, %
                    {{round(X),round(Y-1)},gb_trees:lookup({X,Y-1},Terrain_map2)}, %
                    {{round(X),round(Y+1)},gb_trees:lookup({X,Y+1},Terrain_map2)}, %
                    {{round(X-1),round(Y)},gb_trees:lookup({round(X-1),round(Y)},Terrain_map2)}, %
                    {{round(X+1),round(Y)},gb_trees:lookup({round(X+1),round(Y)},Terrain_map2)} %
                }
                || {{X,Y},Mid_height} <- Diamond_mids],

    Diamond_corners = lists:append([diamond_square:square_step(disq,Vary_list,Diamond) || Diamond <- Diamonds]),
    % TODO send tree to file, and then access from python. I may need to move all this to the windows side.
    update_map(Diamond_corners, Terrain_map2);

vary_map(Step,Size,Vary_reduce,Variance,Terrain_map) ->
    % io:format("Step: ~p~n",[Step]),
    % io:format("Variance: ~p~n",[Variance]),
    Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor(0-Variance),floor(Variance))),
    % Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor((0-Variance)*10),floor(Variance*10))),
    % io:format("~p~n",[Vary_list]),
    % io:format("terrain_map: ~p~n",[Terrain_map]),
    Squares = [{
                    {{X,Y},gb_trees:lookup({X,Y},Terrain_map)},
                    {{X+Step,Y},gb_trees:lookup({X+Step,Y},Terrain_map)},
                    {{X,Y+Step},gb_trees:lookup({X,Y+Step},Terrain_map)},
                    {{X+Step,Y+Step},gb_trees:lookup({X+Step,Y+Step},Terrain_map)}
                } 
                || X <- lists:seq(0,Size-1,Step), Y <- lists:seq(0,Size-1,Step)],
    % io:format("squares: ~p~n",[Squares]),
    Diamond_mids = [diamond_square:diamond_step(disq,Vary_list,Square) || Square <- Squares],
    Terrain_map2 = update_map(Diamond_mids,Terrain_map),
    Diamonds = [{
                    {{round(X-Step/2),round(Y-Step/2)},gb_trees:lookup({round(X-Step/2),round(Y-Step/2)},Terrain_map2)}, %
                    {{round(X+Step/2),round(Y-Step/2)},gb_trees:lookup({round(X+Step/2),round(Y-Step/2)},Terrain_map2)}, %
                    {{round(X-Step/2),round(Y+Step/2)},gb_trees:lookup({round(X-Step/2),round(Y+Step/2)},Terrain_map2)}, %
                    {{round(X+Step/2),round(Y+Step/2)},gb_trees:lookup({round(X+Step/2),round(Y+Step/2)},Terrain_map2)}, %
                    {{X,Y},{value,{Mid_height,0}}}, %
                    {{round(X),round(Y-Step)},gb_trees:lookup({X,Y-Step},Terrain_map2)}, %
                    {{round(X),round(Y+Step)},gb_trees:lookup({X,Y+Step},Terrain_map2)}, %
                    {{round(X-Step),round(Y)},gb_trees:lookup({round(X-Step),round(Y)},Terrain_map2)}, %
                    {{round(X+Step),round(Y)},gb_trees:lookup({round(X+Step),round(Y)},Terrain_map2)} %
                }
                || {{X,Y},Mid_height} <- Diamond_mids],

    Diamond_corners = lists:append([diamond_square:square_step(disq,Vary_list,Diamond) || Diamond <- Diamonds]),
    Terrain_map3 = update_map(Diamond_corners, Terrain_map2),
    % io:format("~p",[gb_trees:values(Terrain_map3)]),
    % io:format("~p",[Step]),
    vary_map(round(Step/2),Size,Vary_reduce,Variance/Vary_reduce,Terrain_map3).



update_map([],Terrain_map) -> 
    Terrain_map;

update_map([none|Points],Terrain_map) -> 
    % io:format("~p",[["none"]++Points]),
    update_map(Points,Terrain_map);

update_map([{Key,Value}|Points],Terrain_map) ->
    % io:format("~p",[[{Key,Value}]++Points]),
    case gb_trees:lookup(Key,Terrain_map) =:= {value,{0,0}} of 
        true -> update_map(Points,gb_trees:update(Key,{round(Value*100)/100,0},Terrain_map));
        _Else -> update_map(Points,Terrain_map)
    end.


update_structure_map([Point|Points],Square,Structure,Map) -> 
    case gb_trees:lookup(Point,Map) of
        none -> Map;
        _Else -> update_structure_map(Points,Square,Structure,Map)
    end;

update_structure_map([],{T_left,T_right,B_left,B_right},Structure,Map) -> 
% update_structure_map({T_left,T_right,B_left,B_right},Structure,Map) -> 
    TL_height = get_height(gb_trees:lookup(T_left,Map)),
    TR_height = get_height(gb_trees:lookup(T_right,Map)),
    BL_height = get_height(gb_trees:lookup(B_left,Map)),
    BR_height = get_height(gb_trees:lookup(B_right,Map)),
    Map1 = gb_trees:update(T_left,{TL_height,Structure},Map),
    Map2 = gb_trees:update(T_right,{TR_height,Structure},Map1),
    Map3 = gb_trees:update(B_left,{BL_height,Structure},Map2),
    gb_trees:update(B_right,{BR_height,Structure},Map3).

get_height({_,{Height,_}}) -> Height;
get_height({_,Height}) -> Height.