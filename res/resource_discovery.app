%% Copyright
{application, resource_discovery, [
  {description, "A Resource Discovery Application"},
  {vsn, "1"},
  {modules, [rd_app, rd_sup, resource_discovery]},
  {registered, [rd_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {rd_app, []}},
  {env, []}
]}.
