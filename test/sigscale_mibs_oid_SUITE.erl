%%% sigscale_mibs_oid_SUITE.erl
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
%%%  @doc Test suite for SNMP MIB OID translations in the
%%% 	{@link //sigscale_mibs. sigscale_mibs} application.
%%%
-module(sigscale_mibs_oid_SUITE).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(sigscalePEN, 50386).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	Port = rand:uniform(32767) + 32768,
	[{userdata, [{doc, "Test suite for SNMP agent MIB OID translations"}]},
	{require, snmp_mgr_agent, snmp},
	{default_config, snmp,
      	[{start_agent, true},
			{agent_udp, Port},
			{agent_engine_id, engine_id()},
			{users,
					[{ocs_mibs_test, [snmpm_user_default, []]}]},
			{managed_agents,
					[{ocs_mibs_test, [ocs_mibs_test, {127,0,0,1}, Port, []]}]}]},
	{require, snmp_app},
	{default_config, snmp_app,
			[{manager,
					[{config, [{verbosity, silence}]},
					{server, [{verbosity, silence}]},
					{net_if, [{verbosity, silence}]}]},
			{agent,
					[{config, [{verbosity, silence}]},
					{agent_verbosity, silence},
					{net_if, [{verbosity, silence}]}]}]},
	{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
	ok = application:start(sigscale_mibs),
	DataDir = filename:absname(?config(data_dir, Config)),
	TestDir = filename:dirname(DataDir),
	BuildDir = filename:dirname(TestDir),
	MibDir =  BuildDir ++ "/priv/mibs/",
	Mibs = [MibDir ++ "SIGSCALE-SMI",
			MibDir ++ "SIGSCALE-TC",
			MibDir ++ "SIGSCALE-MODULES-MIB",
			MibDir ++ "SIGSCALE-PRODUCTS-MIB"],
	ok = ct_snmp:load_mibs(Mibs),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
   ok = sigscale_mib:unload(),
   ok = application:stop(sigscale_mibs),
   ok = ct_snmp:stop(Config).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[smi, modules, textual_conventions, products].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

smi() ->
	[{userdata, [{doc, "Get SigScale enterprise OIDs"}]}].

smi(_Config) ->
	{value, Enterprises} = snmpa:name_to_oid(enterprises),
	SigScale = Enterprises ++ [?sigscalePEN],
	{value, SigScale} = snmpa:name_to_oid(sigscale),
	{value, Products} = snmpa:name_to_oid(sigscaleProducts),
	true = lists:prefix(SigScale, Products),
	{value, Modules} = snmpa:name_to_oid(sigscaleModules),
	true = lists:prefix(SigScale, Modules),
	{value, AgentCapability} = snmpa:name_to_oid(sigscaleAgentCapability),
	true = lists:prefix(SigScale, AgentCapability),
	{value, Management} = snmpa:name_to_oid(sigscaleManagement),
	true = lists:prefix(SigScale, Management),
	{value, Experiment} = snmpa:name_to_oid(sigscaleExperiment),
	true = lists:prefix(SigScale, Experiment).

modules() ->
	[{userdata, [{doc, "Get SigScale modules OIDs"}]}].

modules(_Config) ->
	{value, Modules} = snmpa:name_to_oid(sigscaleModules),
	{value, ModulesMIB} = snmpa:name_to_oid(sigscaleModulesMIB),
	true = lists:prefix(Modules, ModulesMIB),
	{value, SMI} = snmpa:name_to_oid(sigscaleSMI),
	true = lists:prefix(Modules, SMI),
	{value, ProductsMIB} = snmpa:name_to_oid(sigscaleProductsMIB),
	true = lists:prefix(Modules, ProductsMIB),
	{value, Management} = snmpa:name_to_oid(sigscaleManagement),
	{value, TC} = snmpa:name_to_oid(sigscaleTextualConvention),
	true = lists:prefix(Management, TC),
	{value, OcsMIB} = snmpa:name_to_oid(ocsMIB),
	true = lists:prefix(Management, OcsMIB),
	{value, Experiment} = snmpa:name_to_oid(sigscaleExperiment),
	{value, DbpMIB} = snmpa:name_to_oid(diameterBaseProtocolMIB),
	true = lists:prefix(Experiment, DbpMIB),
	{value, DccaMIB} = snmpa:name_to_oid(diameterCCAMIB),
	true = lists:prefix(Experiment, DccaMIB).

textual_conventions() ->
	[{userdata, [{doc, "Get SigScale textual convention OIDs"}]}].

textual_conventions(_Config) ->
	{value, Management} = snmpa:name_to_oid(sigscaleManagement),
	{value, TC} = snmpa:name_to_oid(sigscaleTextualConvention),
	true = lists:prefix(Management, TC).

products() ->
	[{userdata, [{doc, "Get SigScale product OIDs"}]}].

products(_Config) ->
	{value, Products} = snmpa:name_to_oid(sigscaleProducts),
	{value, OCS} = snmpa:name_to_oid(ocs),
	true = lists:prefix(Products, OCS).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% RFC3411 SnmpEngineID using random octets
engine_id() ->
	PEN = <<1:1, ?sigscalePEN:31>>,
	engine_id(binary_to_list(PEN), []).
engine_id(PEN, Acc) when length(Acc) == 27 ->
   PEN ++ [5 | Acc];
engine_id(PEN, Acc) ->
   engine_id(PEN, [rand:uniform(255) | Acc]).

