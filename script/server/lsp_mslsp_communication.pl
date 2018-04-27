:- module(lsp_mslsp_communication, [
   lsp_next_request/2,
   lsp_write_response/2
]).

:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- use_module(lsp_logging).
/** <module> The communication layer of the language server

This module handels the communication with the client application.

@see https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md
@author Lukas Leppich <lukas.leppich@gmail.com>
*/


/**
 * lsp_next_request(+Stream:stream, -Request:json)
 *
 * Read the next request from the stream.
 *
 * @arg Stream The input stream to read from.
 * @arg Request The request as json.
 */
lsp_next_request(Stream, Request) :-
   read_header(Stream, Headers),
   get_content_length(Headers, Length),
   read_string(Stream, Length, Content),
   %lsp_log(debug, Content),
   atom_json_term(Content, Request, []).

/**
 * lsp_write_response(+Stream:stream, +Response:json)
 *
 * Write the provided response to the output stream.
 *
 * @arg Stream The output stream.
 * @arg Response The response to write.
 */
lsp_write_response(Stream, Response) :-
   atom_json_term(Text, Response, [as(string), width(0)]),
   string_length(Text, Length),
   %lsp_log(['Write (', Length, '): ', Text]),
   format(Stream, 'Content-Length: ~d\r\n\r\n~w', [Length, Text]),
   flush_output(Stream), flush_output.

/**
 * read_header(+Stream:stream, -Lines:list)
 *
 * Wait until a new header line is provided by the stream.
 * Read until an empty line is received.
 *
 * @see https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md
 * @arg Stream The input stream.
 * @arg Lines The header lines read from the stream.
 */
read_header(Stream, Lines) :-
   lsp_log(debug, 'wait for input ...'),
   wait_for_input([Stream], [ReadyStream], infinite),
   read_line_to_codes(ReadyStream, Codes),
   \+ at_end_of_stream(Stream),
   string_codes(Line, Codes),
   %string_length(Line, LineLength),
   %lsp_log(debug, ['New input line: ', Line]),
   %lsp_log(debug, ['Length: ', LineLength]),
   ( string_length(Line, 0) ->
     Lines = []
   ; read_header(Stream, T),
     append([Line], T, Lines) ).

/**
 * get_content_length(+Lines:list, -Length:int)
 *
 * Try to read the content length from the proviced header lines.
 *
 * @arg Lines The header lines as a list of strings.
 * @arg Length The content length.
 */
get_content_length([], Length) :-
   Length = 0.

get_content_length([Line|Headers], Length) :-
   string_codes(Line, Codes),
   ( phrase(content_length(Length), Codes) -> true
   ; get_content_length(Headers, Length) ).

% The DCG to read the content length.
content_length(Length) -->
   "Content-Length: ", integer(Length).

/**
 * read_string(+Stream:stream, +Length:int, -Content:string)
 *
 * Read Length characters from the input stream.
 *
 * @arg Stream The input stream.
 * @arg Length The character count to read.
 * @arg Content The content read from the stream.
 */
read_string(_, 0, '') :-
   !.

read_string(Stream, Length, Content) :-
   get_char(Stream, Char),
   NewLength is Length - 1,
   read_string(Stream, NewLength, Next),
   atomic_concat(Char, Next, Content).

