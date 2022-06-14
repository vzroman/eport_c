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
#ifndef eport_c_log__h
#define eport_c_log__h

typedef enum {
    EPORT_C_LOGLEVEL_TRACE = 0,
    EPORT_C_LOGLEVEL_DEBUG = 1,
    EPORT_C_LOGLEVEL_INFO = 2,
    EPORT_C_LOGLEVEL_WARNING = 3,
    EPORT_C_LOGLEVEL_ERROR = 4,
    EPORT_C_LOGLEVEL_FATAL = 5
} EPORT_C_LOGLEVEL;

extern int eport_c_loglevel;

#define LOGLEVEL(level) int eport_c_loglevel = level

#define LOGTRACE(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_TRACE){ fprintf(stdout,"TRACE: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)
#define LOGDEBUG(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_DEBUG){ fprintf(stdout,"DEBUG: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)
#define LOGINFO(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_INFO){ fprintf(stdout,"INFO: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)
#define LOGWARNING(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_WARNING){ fprintf(stdout,"WARNING: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)
#define LOGERROR(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_ERROR){ fprintf(stdout,"ERROR: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)
#define LOGFATAL(...) do{ if(eport_c_loglevel <= EPORT_C_LOGLEVEL_FATAL){ fprintf(stdout,"FATAL: "); fprintf(stdout,__VA_ARGS__); fprintf(stdout,"\r\n");} } while(0)

#define SETLOGLEVEL(level) do{ eport_c_loglevel = level; } while(0)

#endif


