
-module(dynamic_sup).
-behaviour(supervisor).


%%@private
-export([init/1]).
%%API functions
% -export([start/0,start/1,start/3,add_child/4,remove_child/2,set_disq_servers/0]).
-export([start/0,add_child/4,remove_child/2,set_disq_servers/0]).

%%%===================================================================
%%% Public API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created,
%% it is registered locally under the module name,
%% and there is no startup information, such as an
%% initial list of children to start.
%%
%%
%% @end
%%--------------------------------------------------------------------
start()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).
    % set_disq_servers().

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created
%% and it is registered locally under the module name.
%%
%%
%% @end
%%--------------------------------------------------------------------
% start(Start_info)->
%     supervisor:start_link({local,?MODULE},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there can be many supervisors of this type
%% or if the supervisor is to be registered in any way 
%% but locally.
%%
%%
%% @end
%%--------------------------------------------------------------------
% start(Supervisor_name,Registration_type,Start_info)->
%     supervisor:start_link({Registration_type,Supervisor_name},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used to dynamically add a child to at run-time.
%%
%%
%% @end
%%--------------------------------------------------------------------
add_child(Supervisor_name,Child_name,Child_module,Child_type)->
    dist:add(Child_name),
    supervisor:start_child(Supervisor_name,#{id => Child_name,
                                            start => {Child_module,start,[local,Child_name,[]]},
                                            restart => permanent,
                                            type => Child_type,
                                            modules => [Child_module]}).%generate_spec(Child_module,Child_name,Child_type)).


remove_child(Supervisor_name,Child_name)->
    dist:remove(Child_name),
    supervisor:terminate_child(Supervisor_name,Child_name),
    supervisor:delete_child(Supervisor_name,Child_name).

set_disq_servers() ->
    [add_child(dynamic_sup,list_to_atom(string:concat("disq_",Count)),diamond_square,worker) || Count <- ["1","2","3","4","5"]].
%% Mandatory callback functions


%% Modify this function to do appropriate supervision initialization.
%%@private
init(Start_info) ->
    %% This function has a value that is a tuple
    %% consisting of ok, a description of how to 
    %% interact with its children, and a list of its children. It is missing
    %% the restart strategy, intensity and period you intend to use.
    %% this template also starts no children of its own. The information
    %% to do so would come from Start_info if any children are to be 
    %% started.
    %%
    %% The pattern for the value of this function is, 
    %% {ok,{{restart_strategy,intensity,period},children}}
    ChildSpecs = [],
    {ok,{{one_for_one,1000,5},ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Modify this function to include an appropriate shutdown time, etc depending on which portions
%% of the child specification need to be set.
%%@private
% generate_spec(Module,Name,Type)->
% %%
% %% A child Specification is a record with the following mappings.
% %%
% %% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
% %%                  start => mfargs(),      % mandatory. The module's startup function.
% %%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
% %%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
% %%                  type => atom(),                 % optional. Options are worker or supervisor.
% %%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
% %%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
% %%                                                  % such a list is unknown, for example when the child is a 
% %%                                                  % gen_event manager with some unknown types of gen_event handler
% %%                                                  % modules to be added later.
%         #{id => Name,
%           start => {Module,start,[local,Name,[]]},% This template forces local registration of the child and
%                                                   % forces it to startup without parameters. 
%           restart => transient,
%           shutdown => 2000,
%           type => Type,
%           modules => [Module]}.
