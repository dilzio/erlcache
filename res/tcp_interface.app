%% Copyright
{application, tcp_interface, [
  {description, "TCP text frontend to simplecache"},
  {vsn, "1"},
  {modules, [ti_app, ti_sup, ti_server]},
  {registered, [rd_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {ti_app, []}},
  {env, []}
]}.
