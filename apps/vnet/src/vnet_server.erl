-module(vnet_server).
-behaviour(gen_server).
-include ("vnet.hrl").

%% API.
-export([start_link/1]).

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

-spec start_link(Socket :: term()) -> {ok, pid()}.
start_link(Socket) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

%% gen_server.

init([Socket]) ->
  process_flag(trap_exit, true),
  ?INFO("~p: Socket server started...~n", [?MODULE]),
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  rand:seed(exs1024, {A, B, C}),
  gen_server:cast(self(), accept),
  {ok, #state{sock=Socket}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(accept, State=#state{sock=Sock}) ->
  {ok, Accept} = gen_tcp:accept(Sock),
  %% start a new acceptor
  vnet_sup:start_socket(),
  send(Accept, [{hello, <<"Versa TCP Server">>}]),
  {noreply, State#state{sock=Accept}};

handle_cast(exchange, State=#state{sock=Sock}) ->
  send(Sock, [{exchange, start}, {data, []}]),
  {noreply, State};

handle_cast(_Msg, State=#state{sock=Sock}) ->
  send(Sock, [{error, enocomprehend}]),
  {noreply, State}.

handle_info({tcp, _Sock, Payload}, State) ->
  %% send cast to reduce the logic clutter here
  case bert:decode(Payload) of
    [{hello, _}] ->
      gen_server:cast(self(), exchange);
    _ ->
      gen_server:cast(self(), error)
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

