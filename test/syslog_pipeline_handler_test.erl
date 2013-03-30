-module(syslog_pipeline_handler_test).

-include ("../src/syslog_pipeline.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (FRAME, <<"182 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_5m val=0.00\n183 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_15m val=0.00\n">>).

handle_test()->
  syslog_drain_handler:handle(?FRAME, #drain_opts{
    feedback=true,
    parsers=[syslog_message_keyvalue],
    routes=[syslog_pipeline_stdout_emitter]
  }),
  ok.
