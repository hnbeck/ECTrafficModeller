%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(strings)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(pcre)).
:- use_module(library(base64)).
:- use_module(library(sgml)).
:- use_module(library(ssl)).
:- use_module(library(apply)).
:- use_module(library(pprint)).
:- use_module(library(gensym)).

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).


:- initialization((
	logtalk_make(clean),
	set_logtalk_flag(debug, on),
	
	logtalk_load(os(loader)),
	logtalk_load(optionals(loader)),
	logtalk_load(debug_messages(loader)),
	logtalk_load(options(loader)),
	logtalk_library_path(ecmodeller, D),
	format('Directory is ~w~n', [D]),
	logtalk_library_path(logics, D2),
	format('Sourcen is ~w~n', [D2]),
	logtalk_load(logics('xmpp/xmpp_jid_c')),
	logtalk_load(logics('xmpp/xmpp_config')),
	logtalk_load(logics('xmpp/xmpp_xml_p')),
	logtalk_load(logics('xmpp/xmpp_xml')),
	logtalk_load(logics('xmpp/xmpp_sec')),
	logtalk_load(logics('xmpp/xmpp_core')),
	os::change_directory(D2),
	debug_messages::enable(xmpp)
	
)).
