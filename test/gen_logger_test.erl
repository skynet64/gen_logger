%%-----------------------------------------------------------------------------
%% Copyright (c) 2015, Feng Lee <feng@emqtt.io>
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
%%------------------------------------------------------------------------------
-module(gen_logger_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

debug_test() ->
    [Logger:debug("debug msg") || Logger <- loggers()],
    [Logger:debug("debug ~p", ["msg"]) || Logger <- loggers()].

info_test() ->
    [Logger:info("info msg") || Logger <- loggers()],
    [Logger:info("info ~p", ["msg"]) || Logger <- loggers()].

warning_test() ->
    [Logger:warning("warning msg") || Logger <- loggers()],
    [Logger:warning("warning ~p", ["msg"]) || Logger <- loggers()].

error_test() ->
    [Logger:error("error msg") || Logger <- loggers()],
    [Logger:error("error ~p", ["msg"]) || Logger <- loggers()].

critical_test() ->
    [Logger:critical("critical msg") || Logger <- loggers()],
    [Logger:critical("critical ~p", ["msg"]) || Logger <- loggers()].

loggers() ->
    [gen_logger:new(info), gen_logger:new({error_logger, warning}), gen_logger:new(stdout, debug)].

-endif.
