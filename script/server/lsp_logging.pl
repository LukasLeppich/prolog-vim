:- module(lsp_logging, [
      lsp_log/1, 
      lsp_log/2, 
      lsp_log_output_file/1, 
      lsp_log_activate_level/1, 
      lsp_log_deactivate_level/1,
      lsp_get_log_stream/1,
      lsp_get_log_file/1
   ]).

:- use_module(lsp_storage).
/** <module> The logging module.

Provide logging functions for the language server.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_log_output_file(+File:string)
 *
 * Set the output file for log messages.
 *
 * @arg File The logging output file.
 */
lsp_log_output_file(File) :-
   lsp_set_variables([logging_file:File]).

/**
 * lsp_log_activate_level(+Level:atom)
 *
 * Activate a logging level.
 *
 * @arg Level The logging level to activate.
 */
lsp_log_activate_level(Level) :-
   lsp_add_to_list([logging_levels:Level]).

/**
 * lsp_log_deactivate_level(+Level:atom)
 *
 * Deactivate a logging level.
 *
 * @arg Level The logging level to deactivate.
 */
lsp_log_deactivate_level(Level) :-
   lsp_remove_from_list([logging_levels:Level]).

/**
 * lsp_log(+Message:string/list)
 *
 * Log a message to the output file if the default log level (debug) is active.
 *
 * @arg Message A string or list of strings to log.
 */
lsp_log(Message) :-
   lsp_log(debug, Message).

/**
 * lsp_log(+Level:atom, +Message:string/list)
 *
 * Log a message to the output file if the specified log level is active.
 * 
 * @arg Level The log level of the message.
 * @arg Message The message to log.
 */
lsp_log(Level, Message) :-
   \+ is_list(Message),
   lsp_log(Level, [Message]).

lsp_log(Level, Message) :-
   is_list(Message),
   ( is_level_active(Level) ->
     log_message(Level, Message)
   ; true ).

/**
 * lsp_get_log_file(-File:string)
 *
 * Get the current log file.
 *
 * @arg File The current log file.
 */
lsp_get_log_file(File) :-
   lsp_get_variables([logging_file:File]).

/**
 * lsp_get_log_stream(-Stream:stream)
 *
 * Return an output stream of the current log file. Has to be closed!
 *
 * @arg Stream The output stream of the current log file. Has to be closed!
 */
lsp_get_log_stream(Stream) :-
   lsp_get_variables([logging_file:File]),
   open(File, append, Stream).

/**
 * is_level_active(+Level:atom)
 *
 * True if the level is active.
 *
 * @arg Level The level to check.
 */
is_level_active(Level) :-
   lsp_get_variables([logging_levels:Levels]),
   member(Level, Levels).

/**
 * log_message(+Level:atom, +Message:list)
 *
 * Log the provided message to the output stream.
 *
 * @arg Level The log level.
 * @arg Message The message to log.
 */
log_message(Level, Message) :-
   lsp_get_log_stream(Stream),
   catch(
      write_log_message(Stream, Level, Message),
      Error, (
         close(Stream),
         throw(Error)
      )
   ).

/**
 * write_log_message(+Stream:stream, +Level:atom, +Message:list)
 *
 * Write the log message to the provided output stream.
 *
 * @arg Stream The stream to write to.
 * @arg Level The log level.
 * @arg Message The message to log.
 */
write_log_message(Stream, Level, Message) :-
   get_time(Now),
   format_time(atom(Time), '%H:%M:%S', Now),
   append([Level, ' ',Time, ': '], Message, LogMessage),
   write_list(Stream, LogMessage),
   write(Stream, '\n'),
   close(Stream).

/**
 * write_list(+Stream:stream, +List:list)
 * 
 * Write the list to the output stream.
 *
 * @arg Stream The stream to write to.
 * @arg List The list of the content.
 */
write_list(_, []).
write_list(Stream, [X|Xs]) :-
    write(Stream, X),
    write_list(Stream, Xs).
    
