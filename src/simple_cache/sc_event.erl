%% An event handler
-module(sc_event).
-author("dilzio").

%% API
-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         lookup/1,
         create/2,
         replace/2,
         delete/1]).


-define(SERVER, ?MODULE).

%these API functions abstract the actual calls to gen_event for starting, adding/deleting handlers
start_link()->
  gen_event:start_link({local, ?SERVER}).   %the event handler is registered under the module name: ?SERVER

add_handler(Handler, Args)->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args)->
  gen_event:delete_handler(?SERVER, Handler, Args).

%these are business friendly notifcation API's
lookup(Key)->
  gen_event:notify(?SERVER, {lookup, Key}).

create(Key, Value)->
  gen_event:notify(?SERVER, {create,{Key, Value}}).

replace(Key, Value)->
  gen_event:notify(?SERVER, {replace,{Key, Value}}).

delete(Key) ->
  gen_event:notify(?SERVER, {delete, Key}).

