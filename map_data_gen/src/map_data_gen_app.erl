%%%-------------------------------------------------------------------
%% @doc map_data_gen public API
%% @end
%%%-------------------------------------------------------------------

-module(map_data_gen_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    map_data_gen_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
