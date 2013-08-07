%% @author dilzio
%% @doc @todo Add description to create_tables.


-module(create_tables).
-include("schema.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([init_tables/0,
		 insert_user/3,
		 insert_project/2,
		 bootstrap/0,
		 bootstrap2/0]).

bootstrap() ->
	mnesia:start(),
	init_tables(),
	create_tables:insert_project(simple_cache, "a simple cache application").

bootstrap2() ->
	mnesia:start(),
	mnesia:create_table(project, 	
					  [{index, [description]},  %% declare index on description column
					   {attributes, record_info(fields, project)}
					  ]),
    insert_project("Title1", "Description1"),
    insert_project("Title2", "Description2"),
    insert_project("Title3", "Description3").

init_tables() ->
	mnesia:create_table(user, [{attributes, record_info(fields,user)}]),
	mnesia:create_table(project, [{attributes, record_info(fields, project)}]),
	mnesia:create_table(contributor, [{type, bag}, {attributes, record_info(fields, contributor)}]).

insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
	User = #user{id=Id, name=Name},
	Fun = fun() ->
				  mnesia:write(User),
				  lists:foreach(
						fun(Title) ->
								[#project{title=Title}] = mnesia:read(project, Title),      %% will abort transaction if not found
								mnesia:write(#contributor{user_id=Id, project_title=Title})
						end,		   
				  		ProjectTitles)
		  end,
	mnesia:transaction(Fun).

insert_project(Title, Description) ->
	mnesia:dirty_write(#project{title=Title, description=Description}).