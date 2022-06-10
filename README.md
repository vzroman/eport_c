eport_c
=====

    Erlang library provides a simple way for calling programs written in C from Erlang.
    On successfull compilation the 'priv' direcory will contain:
        * eport_c.a - library that should be statically linked with your C program,
        * include - directory with required header files.
    As an alternative to erl_interface the library uses JSON for data serialization/deserialization.
    On the Erlang side jsx (https://github.com/talentdeficit/jsx.git) is used.
    On the C side cJSON (https://github.com/DaveGamble/cJSON) is used.

API
-----
    The library provides the main function 
        
        eport_loop( callback )

    The callback is a function from your application with signature:

        cJSON *callback(char * command, cJSON * body, char **error)

    When 'eport_loop' is called it enters the listenning loop. It doesn't return until the port is closed. When the request from
    an Elang program arrives it is deserialized to cJSON * sructure and the 'callback' is called. The callback handles the request
    and returns a response as a cJSON structure. That's it.

    To call your C program from Erlang you shuold start it first with:

        {ok, PID}  = eport_c:start_link(<<"/path/to/my/executable">>, <<"some arbitrary name">>)

    Then to call it use:

        {ok, Reply} = eport_c:request(PID, <<"my_command">>, #{ my_argument => 34, another_argument => <<"some value">> })

    When the port is not needed anymore it must be closed with:

        eport_c:stop( PID )

Example C program
-----


Build
-----

    $ ./rebar3 compile