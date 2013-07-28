%% Copyright
{application, simple_cache, [
  {description, "A Simple Caching System"},
  {vsn, "1"},
  {modules, [sc_app, sc_sup]},
  {registered, [sc_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sc_app, []}},
  {env, []}
]}.