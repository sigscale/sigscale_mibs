%%% sigscale_mib.erl
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
%%% @doc This module implements the SigScale Enterprise MIB.
%%%

-module(sigscale_mib).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% API
-export([load/1, unload/1]).

%% SNMP instrumentation
%-export([]).


-spec load (Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason = term().
%% @doc Loads the SigScale Enterprise MIB.
load(Agent) ->
	MibDir = code:priv_dir(sigscale_mibs) ++ "/mibs",
	Mibs = [MibDir ++ "/SIGSCALE-SMI",
			MibDir ++ "/SIGSCALE-TC",
			MibDir ++ "/SIGSCALE-MODULES-MIB",
			MibDir ++ "/SIGSCALE-PRODUCTS-MIB"],
	snmpa:load_mibs(Agent, Mibs).

-spec unload (Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason = term().
%% @doc Unloads the SigScale Enterprise MIB.
unload(Agent) ->
	Mibs = ["SIGSCALE-SMI", "SIGSCALE-TC",
			"SIGSCALE-MODULES-MIB", "SIGSCALE-PRODUCTS-MIB"],
	snmpa:unload_mibs(Agent, Mibs).

