%% MIT License
%%
%% Copyright (c) 2018, Versa Developers.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%%
-module(vnet_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_socket/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Port = os:getenv(<<"VERSA_TCP_PORT">>, 8787),
  {ok, Sock} = gen_tcp:listen(Port, [binary, {active, once}]),
  spawn_link(fun spawn_listeners/0),

  Server = {server,
            {vnet_server, start_link, [Sock]},
            temporary, 1000, worker, [vnet_server]},
  Procs = [Server],
  {ok, {{simple_one_for_one, 60, 3600}, Procs}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

spawn_listeners() ->
  [start_socket() || _ <- lists:seq(1, 10)],
  ok.

