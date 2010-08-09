%% @author Benjamin Nortier <bjnortier@gmail.com>
%%
%% The testcase resource is the resource for testcase CRUD

-module(testcase_resource).
-export([init/1]).
-export([allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         provide_content/2,
         accept_content/2
        ]).

-record(context, {doc, metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_ConfigProps) ->
    {{trace, "/tmp"}, #context{}}.
    %%{ok, #context{}}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'PUT', 'DELETE'], ReqData, Context}.

doc_id(ReqData) ->
    Path = wrq:disp_path(ReqData),
    WithoutLeading = case hd(Path) of 
                         "/" -> tl(Path);
                         _ -> Path
                     end,
    %% URL encoding: "testcase/foo" -> "test%2Ffoo"
    "testcase%2F" ++ WithoutLeading.

resource_exists(ReqData, Context) ->
    %% The testcase document in CouchDB has the id test/{{ name of testcase }}
    Id = doc_id(ReqData),
    {json, JSON} = erlang_couchdb:retrieve_document({"localhost", 5984}, "pilgrim", Id),
    case JSON of
        {struct,[{<<"error">>,<<"not_found">>},
                 {<<"reason">>,<<"missing">>}]} -> 
            {false, ReqData, Context};
        Doc -> 
            %% Add the doc to the context so we don't have to fetch twice
            {true, ReqData, Context#context{doc = Doc}}
    end.

content_types_provided(ReqData, Context) ->
    CT = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
    {[{CT, provide_content}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

content_types_accepted(ReqData, Context) ->
    CT = case wrq:get_req_header("content-type", ReqData) of
             undefined -> "application/octet-stream";
             X -> X
         end,
    {MT, _Params} = webmachine_util:media_type_to_detail(CT),
    {[{MT, accept_content}], ReqData,
     Context#context{metadata=[{'content-type', MT}|Context#context.metadata]}}.

accept_content(ReqData, Context) ->
    _Path = wrq:disp_path(ReqData),
    Value = wrq:req_body(ReqData),
    try mochijson2:decode(Value) of
        JSON -> 
            %% TODO: Verify that the expected fields are all present
            case Context#context.doc of
                undefined ->
                    R = erlang_couchdb:create_document({"localhost", 5984}, "pilgrim", doc_id(ReqData), 
                                                   [{<<"expected">>, Value}]),
                    io:format("new: ~p~n", [R]),
                    {true, ReqData, Context};
                {struct, Props} ->
                    NewDoc = lists:keyreplace(<<"expected">>, 1, Props, {<<"expected">>, Value}),
                    R = erlang_couchdb:update_document({"localhost", 5984}, "pilgrim", doc_id(ReqData), NewDoc),
                    io:format("existing: ~p~n", [R]),
                    {true, ReqData, Context};
                _ ->
                    {error, ReqData, Context}
            end

    catch
        _:_ ->  {{halt, 400}, wrq:set_resp_body("The content is not valid JSON", ReqData), Context}
    end.  


delete_resource(ReqData, Context) ->
    {false, ReqData, Context}.
    %% case file:delete(file_path(
    %%                    Context, wrq:disp_path(ReqData))) of
    %%     ok -> {true, ReqData, Context};
    %%     _ -> {false, ReqData, Context}
    %% end.

provide_content(ReqData, Context) ->
    case Context#context.doc of
        undefined ->
            {error, ReqData, Context};
        Doc ->
            {mochijson2:encode(Doc), ReqData, Context}
    end.

