-module(vnet_client).
-behaviour(gen_server).
-include ("vnet.hrl").

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          sock
}).

-define (SERVER, ?MODULE).

%% API.

-spec start_link(IP :: term(), Port :: integer()) -> {ok, pid()}.
start_link(IP, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [IP, Port], []).

%% gen_server.

init([IP, Port]) ->
  process_flag(trap_exit, true),
  ?INFO("~p: connecting to ~p, ~p~n", [?MODULE, IP, Port]),
  {ok, Socket} = gen_tcp:connect(IP, Port, [binary]),
  {ok, #state{sock=Socket}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(hello, State=#state{sock=Sock}) ->
  send(Sock, [{hello, <<"Client">>}]),
  {noreply, State};

handle_cast(exchange, State=#state{sock=Sock}) ->
  send(Sock, [{exchange, start}, {data, []}]),
  {noreply, State};

handle_cast(_Msg, State=#state{sock=Sock}) ->
  send(Sock, [{error, enocomprehend}]),
  {stop, normal, State}.

handle_info({tcp, _Sock, Payload}, State) ->
  %% send cast to reduce the logic clutter here
  % ?INFO("Receiving payload: ~p", [Payload]),
  case bert:decode(Payload) of
    [{hello, Message}] ->
      ?INFO("Received banner from server: ~p~n", [Message]),
      gen_server:cast(self(), hello);
    _ ->
      gen_server:cast(self(), close)
  end,
  {noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Sock, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ============================================================================
%% Private Functions
%%
send(Socket, Payload) ->
  ?INFO("Sending payload: ~p", [Payload]),
  ok = gen_tcp:send(Socket, bert:encode(Payload)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

