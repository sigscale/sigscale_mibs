%%% sigscale_snmp_lib.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements SNMP utilities.
%%%
-module(sigscale_snmp_lib).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

-define(sigscalePEN, 50386).

%% API
-export([engine_id/0]).

-spec engine_id() -> EngineID
	when
		EngineID :: [byte()].
%% @doc Create a unique SNMP EngineID for SigScale Enterprise.
%%
%% 	The algorithm in RFC3411 is used to generate a unique value to
%% 	be used as `snmpEngineID' in an `agent.conf' configuration file
%% 	for the OTP SNMP agent.
%%
engine_id() ->
	PEN = binary_to_list(<<1:1, ?sigscalePEN:31>>),
	case inet:getifaddrs() of
		{ok, IfList} ->
			engine_id1(IfList, PEN, []);
		{error, _Reason} ->
			engine_id4(PEN, [])
	end.
%% @hidden
engine_id1([{_, IfOpt} | T], PEN, Acc) ->
	case lists:keyfind(hwaddr, 1, IfOpt) of
		{hwaddr, [0, 0, 0, 0, 0, 0]} ->
			engine_id1(T, PEN, Acc);
		{hwaddr, [255, 255, 255, 255, 255, 255]} ->
			engine_id1(T, PEN, Acc);
		{hwaddr, HwAddr} when length(HwAddr) == 6 ->
			engine_id1(T, PEN, [HwAddr | Acc]);
		false ->
			engine_id1(T, PEN, Acc)
	end;
engine_id1([], PEN, []) ->
	case inet:getifaddrs() of
		{ok, IfList} ->
			engine_id2(IfList, PEN, []);
		{error, _Reason} ->
			engine_id4(PEN, [])
	end;
engine_id1([], PEN, Acc) ->
	[H | _] = lists:sort(Acc),
	PEN ++ [3 | H].
%% @hidden
%% avoid RFC5735 special-use ipv4 addresses
engine_id2([{_, IfOpt} | T], PEN, Acc) ->
	case lists:keyfind(hwaddr, 1, IfOpt) of
		{addr, {N, _, _, _}} when N == 0; N == 10; N == 127 ->
			engine_id2(T, PEN, Acc);
		{addr, {169, 254, _, _}} ->
			engine_id2(T, PEN, Acc);
		{addr, {172, N, _, _}} when N > 15 ->
			engine_id2(T, PEN, Acc);
		{addr, {192, 0, N, _}} when N == 0; N == 2 ->
			engine_id2(T, PEN, Acc);
		{addr, {192, 88, 99, _}} ->
			engine_id2(T, PEN, Acc);
		{addr, {192, 168, _, _}} ->
			engine_id2(T, PEN, Acc);
		{addr, {198, N, _, _}} when N > 253 ->
			engine_id2(T, PEN, Acc);
		{addr, {198, 51, 100, _}} ->
			engine_id2(T, PEN, Acc);
		{addr, {203, 0, 113, _}} ->
			engine_id2(T, PEN, Acc);
		{addr, {N, _, _, _}} when N > 239 ->
			engine_id2(T, PEN, Acc);
		{addr, {A, B, C, D}} ->
			engine_id2(T, PEN, [[A, B, C, D] | Acc]);
		_ ->
			engine_id2(T, PEN, Acc)
	end;
engine_id2([], PEN, []) ->
	case inet:getifaddrs() of
		{ok, IfList} ->
			engine_id3(IfList, PEN, []);
		{error, _Reason} ->
			engine_id4(PEN, [])
	end;
engine_id2([], PEN, Acc) ->
	[H | _] = lists:sort(Acc),
	PEN ++ [1 | H].
%% @hidden
%% avoid RFC5156 special-use ipv6 addresses
engine_id3([{_, IfOpt} | T], PEN, Acc) ->
	case lists:keyfind(hwaddr, 1, IfOpt) of
		{addr, {0, 0, 0, 0, 0, 0, 0, 1}} ->
			engine_id3(T, PEN, Acc);
		{addr, {0, 0, 0, 0, 0, 65535, _, _}} ->
			engine_id3(T, PEN, Acc);
		{addr, {N, _, _, _, _, _, _, _}} when N > 65199, N < 65216 ->
			engine_id3(T, PEN, Acc);
		{addr, {N, _, _, _, _, _, _, _}} when N > 64511, N < 65024 ->
			engine_id3(T, PEN, Acc);
		{addr, {8193, 3512,_, _, _, _, _, _}} ->
			engine_id3(T, PEN, Acc);
		{addr, {8194, _, _, _, _, _, _, _}} ->
			engine_id3(T, PEN, Acc);
		{addr, {8193, 0, _, _, _, _, _, _}} ->
			engine_id3(T, PEN, Acc);
		{addr, {N, _, _, _, _, _, _, _}} when N > 24319; N < 24576 ->
			engine_id3(T, PEN, Acc);
		{addr, {16382, _, _, _, _, _, _, _}} ->
			engine_id3(T, PEN, Acc);
		{addr, {8193, N, _, _, _, _, _, _}} when N > 4095; N < 4112 ->
			engine_id3(T, PEN, Acc);
		{addr, {0, 0, 0, 0, 0, 0, 0, 0}} ->
			engine_id3(T, PEN, Acc);
		{addr, {N, _, _, _, _, _, _, _}} when N > 65279 ->
			engine_id3(T, PEN, Acc);
		{addr, {A, B, C, D, E, F, G, H}} ->
			engine_id3(T, PEN, [[A, B, C, D, E, F, G, H] | Acc]);
		_ ->
			engine_id3(T, PEN, Acc)
	end;
engine_id3([], PEN, []) ->
	engine_id4(PEN, []);
engine_id3([], PEN, Acc) ->
	[H | _] = lists:sort(Acc),
	A = [[N bsr 8, N band 16#00FF] || N <- H],
	PEN ++ [2 | A].
%% @hidden
engine_id4(PEN, Acc) when length(Acc) == 27 ->
	PEN ++ [5 | Acc];
engine_id4(PEN, Acc) ->
	engine_id4(PEN, [rand:uniform(255) | Acc]).

