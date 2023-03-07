-module(generate_map).
-export([make_map/1]).



-spec make_map(term()) -> term().
make_map(Size_factor) ->
    Size = round(math:pow(2,Size_factor)),
    Variance = Size*10,
    Step = Size,
    % Step = round(Size/2),
    Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(0-Variance,Variance)),
    % io:format("||~p||",[Vary_list]),
    % [{{X,Y},0} || X <- lists:seq(0,Size), Y <- lists:seq(0,Size)].
    % orddict:from_list([{{X,Y},0} || X <- lists:seq(0,Size), Y <- lists:seq(0,Size)]).
    Terrain_map1 = gb_trees:from_orddict(orddict:from_list([{{X,Y},0} || X <- lists:seq(0,Size), Y <- lists:seq(0,Size)])),
    Terrain_map2 = gb_trees:update({0,0},lists:nth(rand:uniform(length(Vary_list)),Vary_list),Terrain_map1),
    Terrain_map3 = gb_trees:update({0,Size},lists:nth(rand:uniform(length(Vary_list)),Vary_list),Terrain_map2),
    Terrain_map4 = gb_trees:update({Size,0},lists:nth(rand:uniform(length(Vary_list)),Vary_list),Terrain_map3),
    Terrain_map5 = gb_trees:update({Size,Size},lists:nth(rand:uniform(length(Vary_list)),Vary_list),Terrain_map4),
    Map_name = save_map(1,
                        string:concat("/home/orindale/Programing/terra_struct_gen/map_creation/maps_datas/Size_", 
                                      integer_to_list(Size)),
                        vary_map(Step,
                                 Size,
                                 Variance,
                                 gb_trees:update({Size,Size},
                                                 lists:nth(rand:uniform(length(Vary_list)),
                                                           Vary_list),
                                                 Terrain_map1))),
    % Run python file
    Python_to_use = "/usr/bin/python ",
    Python_program_path = "/home/orindale/Programing/terra_struct_gen/map_creation/make_map.py ",
    Command = string:concat(string:concat(Python_to_use,Python_program_path),
                            string:concat(string:concat("--map_size ",integer_to_list(Size)),
                                          string:concat(" --map_path ", Map_name))),
    os:cmd(Command).
    % Vary_list. 

save_map(Count,Name,Terrain_map) ->
    % file:write_file(Name, Terrain_map).
    case file:read_file_info(Name) of
        {error, enoent} -> file:write_file(Name, io_lib:fwrite("~p.\n",[Terrain_map])),
                           Name;
        _Else -> New_name = string:concat(Name,string:concat("(",string:concat(integer_to_list(Count),")"))),
                case file:read_file_info(New_name) of
                    {error, enoent} -> file:write_file(New_name, io_lib:fwrite("~p.\n",[Terrain_map])),
                                       New_name;
                    _else -> save_map(Count+1,Name,Terrain_map)
                end
    end.
-spec vary_map(term(),term(),term(),term()) -> term().
vary_map(1,Size,Variance,Terrain_map) -> 
    % io:format("|~p|",[Variance]),
    % Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor((0-Variance)*10),floor(Variance*10))),
    Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor(0-Variance),floor(Variance))),
    % Vary_list = [0.0],
    % io:format("||~p||",[Vary_list]),

    Squares = [{
                    {{X,Y},gb_trees:lookup({X,Y},Terrain_map)},
                    {{X+1,Y},gb_trees:lookup({X+1,Y},Terrain_map)},
                    {{X,Y+1},gb_trees:lookup({X,Y+1},Terrain_map)},
                    {{X+1,Y+1},gb_trees:lookup({X+1,Y+1},Terrain_map)}
                } 
                || X <- lists:seq(0,Size-1,1), Y <- lists:seq(0,Size-1,1)],

    Diamond_mids = [diamond_square:diamond_step(dist:get_next(),Vary_list,Square) || Square <- Squares],
    Terrain_map2 = update_map(Diamond_mids,Terrain_map),
    Diamonds = [{
                    {{round(X-1/2),round(Y-1/2)},gb_trees:lookup({round(X-1/2),round(Y-1/2)},Terrain_map2)}, %
                    {{round(X+1/2),round(Y-1/2)},gb_trees:lookup({round(X+1/2),round(Y-1/2)},Terrain_map2)}, %
                    {{round(X-1/2),round(Y+1/2)},gb_trees:lookup({round(X-1/2),round(Y+1/2)},Terrain_map2)}, %
                    {{round(X+1/2),round(Y+1/2)},gb_trees:lookup({round(X+1/2),round(Y+1/2)},Terrain_map2)}, %
                    {{X,Y},{value,Mid_height}}, %
                    {{round(X),round(Y-1)},gb_trees:lookup({X,Y-1},Terrain_map2)}, %
                    {{round(X),round(Y+1)},gb_trees:lookup({X,Y+1},Terrain_map2)}, %
                    {{round(X-1),round(Y)},gb_trees:lookup({round(X-1),round(Y)},Terrain_map2)}, %
                    {{round(X+1),round(Y)},gb_trees:lookup({round(X+1),round(Y)},Terrain_map2)} %
                }
                || {{X,Y},Mid_height} <- Diamond_mids],

    Diamond_corners = lists:append([diamond_square:square_step(dist:get_next(),Vary_list,Diamond) || Diamond <- Diamonds]),
    % TODO send tree to file, and then access from python. I may need to move all this to the windows side.
    gb_trees:to_list(update_map(Diamond_corners, Terrain_map2));

vary_map(Step, Size, Variance, Terrain_map) ->
    % io:format("~p",[Step]),
    io:format("|~p",[Variance]),
    Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor(0-Variance),floor(Variance))),
    % Vary_list = lists:map(fun(X) -> X/10 end, lists:seq(floor((0-Variance)*10),floor(Variance*10))),
    % io:format("|~p|",[Vary_list]),

    Squares = [{
                    {{X,Y},gb_trees:lookup({X,Y},Terrain_map)},
                    {{X+Step,Y},gb_trees:lookup({X+Step,Y},Terrain_map)},
                    {{X,Y+Step},gb_trees:lookup({X,Y+Step},Terrain_map)},
                    {{X+Step,Y+Step},gb_trees:lookup({X+Step,Y+Step},Terrain_map)}
                } 
                || X <- lists:seq(0,Size-1,Step), Y <- lists:seq(0,Size-1,Step)],

    Diamond_mids = [diamond_square:diamond_step(dist:get_next(),Vary_list,Square) || Square <- Squares],
    Terrain_map2 = update_map(Diamond_mids,Terrain_map),
    Diamonds = [{
                    {{round(X-Step/2),round(Y-Step/2)},gb_trees:lookup({round(X-Step/2),round(Y-Step/2)},Terrain_map2)}, %
                    {{round(X+Step/2),round(Y-Step/2)},gb_trees:lookup({round(X+Step/2),round(Y-Step/2)},Terrain_map2)}, %
                    {{round(X-Step/2),round(Y+Step/2)},gb_trees:lookup({round(X-Step/2),round(Y+Step/2)},Terrain_map2)}, %
                    {{round(X+Step/2),round(Y+Step/2)},gb_trees:lookup({round(X+Step/2),round(Y+Step/2)},Terrain_map2)}, %
                    {{X,Y},{value,Mid_height}}, %
                    {{round(X),round(Y-Step)},gb_trees:lookup({X,Y-Step},Terrain_map2)}, %
                    {{round(X),round(Y+Step)},gb_trees:lookup({X,Y+Step},Terrain_map2)}, %
                    {{round(X-Step),round(Y)},gb_trees:lookup({round(X-Step),round(Y)},Terrain_map2)}, %
                    {{round(X+Step),round(Y)},gb_trees:lookup({round(X+Step),round(Y)},Terrain_map2)} %
                }
                || {{X,Y},Mid_height} <- Diamond_mids],
%%% SHOULD I REMAKE THE VARY_LIST HERE? IT MAY FIX ISSUES WITH EXTREME CHANGES IN THE HEIGHTS.
    Diamond_corners = lists:append([diamond_square:square_step(dist:get_next(),Vary_list,Diamond) || Diamond <- Diamonds]),
    Terrain_map3 = update_map(Diamond_corners, Terrain_map2),
    % io:format("~p",[gb_trees:values(Terrain_map3)]),
    % io:format("~p",[Step]),
    vary_map(round(Step/2),Size,Variance/2,Terrain_map3).



update_map([],Terrain_map) -> 
    Terrain_map;

update_map([none|Points],Terrain_map) -> 
    % io:format("~p",[["none"]++Points]),
    update_map(Points,Terrain_map);

update_map([{Key,Value}|Points],Terrain_map) ->
    % io:format("~p",[[{Key,Value}]++Points]),
    case gb_trees:lookup(Key,Terrain_map) =:= {value,0} of 
        true -> update_map(Points,gb_trees:update(Key,round(Value*100)/100,Terrain_map));
        _Else -> update_map(Points,Terrain_map)
    end.

