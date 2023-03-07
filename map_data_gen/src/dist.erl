-module(dist).
-behaviour(gen_server).
-define(SERVER,?MODULE).


% API commands
-export([start/0,start/2,start/3,stop/0,add/1,remove/1,get_next/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
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
start(Name,Registration_type,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, [], []).


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
%% Adds a process to the distributer
%%
%% @end
%%--------------------------------------------------------------------
-spec add(term()) -> term().
add(Name) -> gen_server:call(?MODULE, {add,Name}).


%%--------------------------------------------------------------------
%% @doc
%% Removes a process from the distributer
%%
%% @end
%%--------------------------------------------------------------------
-spec remove(term()) -> term().
remove(Name) -> gen_server:call(?MODULE,Name).


%%--------------------------------------------------------------------
%% @doc
%% Gets the next process from the distributer
%%
%% @end
%%--------------------------------------------------------------------
-spec get_next() -> term().
get_next() -> gen_server:call(?MODULE,get_next).


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
        {ok,[]}.
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
        replace_stopped,
        down}; %% setting the server's internal state to down

handle_call({add,Process},_From,State) ->
    {reply,
        process_added,
        State++[Process]};

handle_call({remove,Process},_From,State) ->
    {reply,
        process_removed,
        State -- [Process]};

handle_call(get_next,_From,[H|T]) ->
    {reply,
        H,
        T++[H]}.

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