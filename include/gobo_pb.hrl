%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Heroku, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% Current verbs
-define(CHECKIN, 0).
-define(GET, 1).
-define(SET, 2).
-define(DEL, 3).
-define(ESET, 4).
-define(SNAP, 5).
-define(DELSNAP, 6).
-define(NOOP, 7).
-define(WATCH, 8).
-define(WALK, 9).
-define(CANCEL, 10).
-define(GETDIR, 14).
-define(STAT, 16).

%% Future verbs
-define(MONITOR, 11).
-define(SYNCPATH, 12).

-define(EOTHER, 127).
-define(ETAG_IN_USE, 1).
-define(EUNKNOWN_VERB, 2).
-define(EREDIRECT, 3).
-define(EINVALID_SNAP, 4).
-define(ECAS_MISMATCH, 5).
-define(EBAD_PATH, 6).
-define(EMISSING_ARG, 7).
-define(ENOTDIR, 20).
-define(EISDIR, 21).
-define(ENOENT, 22).
