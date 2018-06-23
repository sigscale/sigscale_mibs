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

-spec engine_id() -> Result
	when
		Result :: EngineID | {error, Reason},
		EngineID :: [byte()],
		Reason :: term().
%% @doc Create a unique SNMP EngineID for SigScale Enterprise.
%%
%% 	The algorithm in RFC3411 is used to generate a unique value to
%% 	be used as `snmpEngineID' in an `agent.conf' configuration file
%% 	for the OTP SNMP agent.
%%
engine_id() ->
	case inet:getifaddrs() of
		{ok, IfList} ->
			B = <<1:1, 0:31, ?sigscalePEN>>,
			engine_id(IfList, binary_to_list(B));
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
engine_id(_, Acc) when length(Acc) >= 12 ->
	lists:sublist(Acc, 12);
engine_id([{_, IfOpt} | T], Acc) when length(Acc) < 12 ->
	case lists:keyfind(hwaddr, 1, IfOpt) of
		{_, HwAddr} ->
			engine_id(T, Acc ++ HwAddr);
		false ->
			engine_id(T, Acc)
	end;
engine_id([], Acc) ->
	Acc ++ [rand:uniform(255)].

