%% Copyright
{application, resource_discovery, [
  {description, "A Resource Disovery Application"},
  {vsn, "1"},
  {modules, [rd_app, rd_sup]},
  {registered, [rd_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {rd_app, []}},
  {env, []}
]}.