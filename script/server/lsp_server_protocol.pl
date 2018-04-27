:- module(lsp_server_protocol, [lsp_start/2]).

:- use_module(lsp_logging).
:- use_module(lsp_json_utils).
:- use_module(lsp_storage).
:- use_module(lsp_mslsp_communication).
:- use_module(lsp_request_handler).
/** <module> The server main-loop 

This module contains the server main loop.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_start(+InputStream:stream, +OutputStream:stream)
 * 
 * Starts the main server loop using the provided streams
 * for communication.
 * 
 * @arg InputStream The stream to read requests from.
 * @arg OutputStream The output stream for the responses.
 */
lsp_start(InputStream, OutputStream) :-
   lsp_log(debug, 'Start main server loop'),
   repeat,
   lsp_log(debug, 'Wait for next request'),
   catch(
      % if true => backtracking with repeat
      % if false => no backtracking, exit 
      \+ process(InputStream, OutputStream),
      error(E, Context),
      (
         lsp_log(error, ['Error: ', E]),
         lsp_log(error, [Context])
      )
   ).

/**
 * process(+InputStream:stream, +OutputStream:stream)
 * 
 * Is used to read one request and write the response if bould. 
 *
 * @arg InputStream The stream to read the request from.
 * @arg OutputStream The output stream for the response.
 */
process(InputStream, OutputStream) :- 
   lsp_next_request(InputStream, Request),
   process_request(Request, Response),
   ( nonvar(Response) ->
     lsp_log(debug, ['Response is bound, send to client: ', Response]),
     lsp_write_response(OutputStream, Response)
   ; lsp_log(debug, ['Response is not bound'])).

/**
 * process_request(+Request:json, -Response:json)
 *
 * Unmarshal the request json and handle the request. 
 * 
 * @arg Request The request as json, it has to contain the fields:
 *   id, method and params.
 * @arg Response THe response as json, can be unbound if the request does not
 *   expect an response.
 */
process_request(Request, Response) :- 
   lsp_get_from_json(Request,
      ['id', 'method', 'params'],
      [ Id,   Method,   Params ]),
   lsp_log(debug, ['Process request: ', Id, ' ', Method]),
   lsp_handle_request(Method, Id, Params, Response).

