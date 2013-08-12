%% Copyright
{application, simple_cache, [
  {description, "A Simple Caching System"},
  {vsn, "0.3.0"},
  {modules, [sc_app, sc_sup, sc_element_sup, sc_store, sc_element, sc_event, sc_event_logger]},
  {registered, [sc_sup, sc_element_sup, sc_event]},
  {applications, [kernel, stdlib, sasl, mnesia, resource_discovery]},
  {mod, {sc_app, []}},
  {env, []}
]}.
