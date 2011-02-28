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
