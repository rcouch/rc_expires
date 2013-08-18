%%% -*- erlang -*-
%%%
%%% This file is part of rc_expires released under the MIT license.
%%% See the NOTICE for more information.

-define(DNAME, <<"_design/_rc_expires">>).

-define(EXPIRES_JS_DDOC, {[{<<"map">>, <<"
    function(doc) {
        if (doc.ttl || doc.timestamp) {
            emit(doc.timestamp, doc);
        }
    }">>
}]}).
