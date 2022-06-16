/*----------------------------------------------------------------
* Copyright (c) 2022 Faceplate
*
* This file is provided to you under the Apache License,
* Version 2.0 (the "License"); you may not use this file
* except in compliance with the License.  You may obtain
* a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "eport_c.h"

int eport_c_loglevel = EPORT_C_LOGLEVEL_INFO;

//--------------protocol-----------------------------------
cJSON *parse_request( const char *request, char **error );
cJSON* on_ok(cJSON *response);
cJSON* on_error(char* text);
cJSON * create_response( cJSON *request, cJSON *response );

//-------------transport----------------------------------
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);
int read_cmd(byte **buf);
int write_cmd(byte *buf);

//-------------Internal methods--------------------------
cJSON *set_log_level(cJSON *args, char **error);

//========================THE LOOP==================================
void eport_loop(eport_request_handler callback){

    byte *request;
    byte *response;
    int len = 0;
    cJSON *requestJSON = NULL;
    cJSON *responseJSON = NULL;

    //-----------the loop--------------------------
    while ( 1 ){
        request = NULL;
        response = NULL;
        LOGTRACE("wait for request or event");
        len = read_cmd( &request );
        if (len == EOF) {
            if (request != NULL){
                free(request);
            }
            break;
        }

        char *error = NULL;
        // parse JSON
        LOGTRACE("parse the request: %s", (char *)request);
        requestJSON = parse_request((char *)request, &error );

        // Handle the request with the callback
        if (requestJSON != NULL){

            cJSON *method = cJSON_GetObjectItemCaseSensitive(requestJSON, "method");
            cJSON *args = cJSON_GetObjectItemCaseSensitive(requestJSON, "args");

            if (strcmp(method->valuestring, "set_log_level") == 0){
                responseJSON = set_log_level(args, &error);
            }else{
                LOGTRACE("call the user callback");
                responseJSON = callback(method->valuestring, args, &error );
                LOGTRACE("return from the user callback");
            }
        }

        if (responseJSON != NULL){
            // wrap the response
            responseJSON = on_ok( responseJSON );

        }else{
            // there must be some error
            if (error == NULL){
                responseJSON = on_error( "undefined error" );
            }else{
                responseJSON = on_error( error );
            }
        }

        // Build the response string
        responseJSON = create_response( requestJSON, responseJSON );
        response =  (byte *)cJSON_PrintUnformatted( responseJSON );

        LOGTRACE("send the response: %s",(char *)response);
        write_cmd( response );

        cJSON_Delete( requestJSON ); requestJSON = NULL;
        cJSON_Delete( responseJSON ); responseJSON = NULL;
        free( request );
        free( response );
    }

    LOGINFO("EXIT port");
}

//-----------------------Protocol handlers------------------------------------------------
cJSON *parse_request( const char *request, char **error ){

    cJSON *requestJSON = cJSON_Parse( request );

    // Parse the request to JSON structure
    if (requestJSON == NULL){
        *error = "invalid request JSON";
        goto error;
    }

    // Parse the type of the request
    cJSON *method = cJSON_GetObjectItemCaseSensitive(requestJSON, "method");
    if (!cJSON_IsString(method) || (method->valuestring == NULL)){
        *error = "undefined request method";
        goto error;
    }

    // Parse transaction ID
    cJSON *tid = cJSON_GetObjectItemCaseSensitive(requestJSON, "tid");
    if ( !cJSON_IsNumber(tid)) {
        *error = "invalid transaction id";
        goto error;
    }

    return requestJSON;

error:
    cJSON_Delete( requestJSON );
    return NULL;
}

cJSON* on_ok(cJSON *response){
    cJSON *result = cJSON_CreateObject();
    cJSON_AddStringToObject(result, "type", "ok");
    cJSON_AddItemToObject(result, "result", response);
    return result;
}

cJSON* on_error(char* text){
    cJSON *result = cJSON_CreateObject();
    cJSON_AddStringToObject(result, "type", "error");
    cJSON_AddStringToObject(result, "text", text);
    return result;
}

// Build the response
cJSON * create_response( cJSON *request, cJSON *response ){
    cJSON *result = cJSON_CreateObject();

    // Inherit command type from the request
    cJSON *method = cJSON_GetObjectItemCaseSensitive(request, "method");
    cJSON_AddStringToObject(result, "method", method->valuestring);

    // Inherit transaction id from the request
    cJSON *tid = cJSON_GetObjectItemCaseSensitive(request, "tid");
    cJSON_AddNumberToObject(result, "tid", tid->valuedouble);

    // Add the response body
    cJSON_AddItemToObject(result, "reply", response);

    return result;
}

//----------Internal methods----------------------------------------
cJSON *set_log_level(cJSON *args, char **error){
    if (!cJSON_IsNumber(args)){
        *error = "level is not defined";
        return NULL;
    }else if(args->valuedouble < EPORT_C_LOGLEVEL_TRACE || args->valuedouble > EPORT_C_LOGLEVEL_FATAL){
        *error = "invalid log level";
        return NULL;
    }else{
        SETLOGLEVEL(args->valuedouble);
        return cJSON_CreateString("ok");
    }
}
//----------Read/Write helpers----------------------------------------
int read_exact(byte *buf, int len) {
    int i, got=0;
    do {
        if ((i = read(IN_DESC, buf+got, len-got)) <= 0){
            return EOF;
        }
        got += i;
    } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len) {
    int i, wrote = 0;
    do {
        if ((i = write(OUT_DESC, buf+wrote, len-wrote)) <= 0)
        return (i);
        wrote += i;
    } while (wrote<len);

    return (len);
}

int read_cmd(byte **buf) {
    byte lbuf[HEADER_LENGTH];
    int len;
    int i;

    // Step 1. Read the length of the message
    if (read_exact(lbuf, HEADER_LENGTH) != HEADER_LENGTH) return(-1);

    // Convert the length buffer to the integer
    len = 0;
    for (i = 0; i < HEADER_LENGTH; i++){
        len = len | (lbuf[i] << (8 * (HEADER_LENGTH - i -1)) );
    }

    // Step 2. Read the message itself
    *buf = malloc(len); // dynamically allocate the memory for the message
    if (*buf == NULL){
        return -1;
    }
    return read_exact(*buf, len);
}

int write_cmd(byte *buf){
    int len = strlen((char *)buf);

    byte lbuf[HEADER_LENGTH];
    int i;

    // Convert the length from integer to the big-endian
    for (i = 0; i < HEADER_LENGTH; i++){
        lbuf[i] = 0xff & ( len >> (8 * (HEADER_LENGTH - i -1)) ) ;
    }

    // Send the response
    write_exact(lbuf, HEADER_LENGTH);
    return write_exact(buf, len);
}

