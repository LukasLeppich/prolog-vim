:- module(lsp_language_server, [start/1]).
:- use_module(lsp_logging).
:- use_module(lsp_server_protocol).
/** <module> Language Server for Prolog

This module is the starting point of the language server for prolog projects.

The server has to be started with the parameters: 

--nodebug --nosignal --quiet

This is due to the fact that the input and output streams are used to
communicate with the client application.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

:- prompt(_, '').
:- set_stream(user_input, type(text)).
:- set_stream(user_input, encoding(utf8)).

/** 
 * start(+LogFile:string)
 * 
 * Main entry point for the prolog server.
 * 
 * @arg LogFile Output file for logging.
 */
start(LogFile) :- 
   lsp_log_output_file(LogFile), 
   lsp_log_activate_level(debug),
   lsp_log('########################################################'),
   lsp_log('##################    Start server    ##################'),
   lsp_log('########################################################'),
   current_prolog_flag(version, Version),
   lsp_log(debug, ['Prolog version: ', Version]),
   lsp_start(user_input, user_output),
   lsp_log('Stop server'),
   halt(0).

