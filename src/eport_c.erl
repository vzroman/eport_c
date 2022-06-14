%%----------------------------------------------------------------
%% Copyright (c) 2022 Faceplate
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%----------------------------------------------------------------
-module(eport_c).

-include("eport_c.hrl").

%%==============================================================================
%%	Control API
%%==============================================================================
-export([
    start_link/2,start_link/3,
    stop/1,
    request/3, request/4
]).

-define(INIT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).
-define(STOP_TIMEOUT,30000).

-define(HEADER_LENGTH,4).

%%==============================================================================
%%	Control API
%%==============================================================================
start_link(App, Name) ->
    start_link(App, Name, #{}).
start_link(App, Name, Options) ->
    Self = self(),
    PID = spawn_link(fun()->init(App, Name, Self, #{
            response_timeout => maps:get(response_timeout, Options, ?RESPONSE_TIMEOUT),
            no_activity_timeout =>  maps:get(no_activity_timeout, Options, infinity)
        }) end),
    receive
        {PID,started}-> {ok,PID};
        {'EXIT', PID, Reason}-> {error, Reason}
    after
        ?INIT_TIMEOUT->
            stop(PID),
            { error, timeout }
    end.

stop(PID) ->
    PID ! { self(), stop }.

request(PID, Method, Args)->
    request(PID, Method, Args, undefined).
request(PID, Method, Args, Timeout)->
    TID = rand:uniform(16#FFFF),
    Request = #{
        <<"method">> => Method,
        <<"tid">> => TID,
        <<"args">> => Args
    },
    PID ! { self(), call, jsx:encode(Request), Timeout },
    wait_for_reply( PID, Method, TID ).

wait_for_reply( PID, Method, TID )->
    receive
        {PID, reply, {ok, Result} }-> 
            case try jsx:decode(Result, [return_maps]) catch _:_->{invalid_json, Result } end of
                #{<<"method">> := Method, <<"tid">> := TID, <<"reply">> := Reply}-> 
                    case Reply of
                        #{<<"type">> := <<"ok">>, <<"result">> := MethodResult}->
                            {ok, MethodResult};
                        #{<<"type">> := <<"error">>, <<"text">> := Error}->
                            {error, Error};
                        Unexpected->
                            {error, {unexpected_port_reply, Unexpected} }
                    end;
                Unexpected->
                    ?LOGWARNING("unexpected reply from the port ~p",[Unexpected]),
                    wait_for_reply( PID, Method, TID )
            end;
        {PID, reply, Error }->
            Error
    end.    

%%==============================================================================
%%	Initialization procedure
%%==============================================================================
init( App, Name, Owner, Options ) ->
    process_flag(trap_exit, true),
    case init_ext_programm( App ) of
        {ok,Port}->
            Owner ! {self(),started},
            loop(Port, Owner, Options#{ name => Name });    
        InitError->
            Owner ! InitError
    end.

init_ext_programm(App)->
    case filelib:is_file(App) of
        true ->
            try 
                Port = open_port({spawn, App}, [{packet, ?HEADER_LENGTH}, binary, nouse_stdio]),
                {ok, Port}
            catch
                _:Error->{error,Error}
            end;
        _ ->
            {error, { application_not_found, App }}
    end.    

%%==============================================================================
%%	THE LOOP
%%==============================================================================
loop( Port, Owner, #{name := Name, response_timeout := ResponseTimeout } = Options ) ->
    NoActivityTimeout = maps:get(no_activity_timeout, Options, infinity),
    receive
        {Owner, call, Msg, OverrideTimeout} ->
            Timeout = 
                if
                    is_integer(OverrideTimeout) -> OverrideTimeout;
                    true -> ResponseTimeout
                end,
            Result = call( Port, Msg, Options, Timeout ),
            Owner ! {self(), reply, Result},  
            loop(Port,Owner,Options);
        {Port, {data, _Data}}->
            ?LOGWARNING("~ts unexpected data is received from the port"),
            loop(Port, Owner, Options);
        { Owner, stop } ->
            ?LOGINFO("~ts stopping port",[Name]),
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    ?LOGINFO("~ts port is closed",[Name]),
                    unlink(Owner),
                    exit(normal)
            after
                ?STOP_TIMEOUT->
                    ?LOGERROR("~ts timeout on closing port",[Name]),
                    port_close( Port ),
                    exit( close_port_timeout )
            end;
        {'EXIT', Port, Reason} ->
            ?LOGINFO("~ts port terminated",[Name]),
            exit({port_terminated, Reason});
        {'EXIT', Owner, Reason} ->
            ?LOGINFO("~ts owner exit with ~p, closing port",[Name, Reason]),
            port_close( Port ),
            exit( Reason );
        Unexpected->
            ?LOGWARNING("~ts port unexpected message ~p",[Name,Unexpected]),
            loop(Port, Owner, Options)
    after
        NoActivityTimeout->
            ?LOGWARNING("~ts no activity, stop the port",[Name]),
            exit( no_activity )
    end.

call( Port, Msg, _Options, Timeout )->
    Port ! {self(), {command, Msg}},
    receive
        {Port, {data, Data}} ->
            {ok, Data }
    after
        Timeout->
            {error, timeout}
    end.





