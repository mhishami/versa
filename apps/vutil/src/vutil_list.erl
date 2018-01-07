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
-module (vutil_list).
-author ('Hisham Ismail <mhishami@gmail.com>').

-export ([from_bin/1]).
-export ([from_integer/1]).
-export ([to_bin/1]).

-spec from_bin(binary()) -> list().
from_bin(Bin) when is_binary(Bin) ->
  erlang:binary_to_list(Bin).

-spec from_integer(integer()) -> list().
from_integer(Int) when is_integer(Int) ->
  erlang:integer_to_list(Int).

-spec to_bin(list()) -> binary().
to_bin(List) when is_list(List) ->
  erlang:list_to_binary(List).
