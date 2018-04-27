:- module(lsp_request_handler, [lsp_handle_request/4]).

:- use_module(lsp_logging).
:- use_module(lsp_json_utils).
:- use_module(lsp_storage).
:- use_module(lsp_tokens).
:- use_module(lsp_autocomplete).
/** <module> Processes the protocol messages.
 
@see https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md
@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_handle_request(+Method:string, +Id:int, +Params:json, -Response:json)
 * 
 * Process a request with the provided method and parameter.
 *
 * The id of the request is required if the method expects an response.
 *
 * @arg Method The method of the request.
 * @arg Id The request id, requirered for the response.
 * @arg Params The parameter for the request.
 * @arg Response The json response if expected by the method.
 */
lsp_handle_request('initialize', Id, Params, Response) :-
   lsp_log(debug, 'Handle initialize request'),
   lsp_get_from_json(Params,
      ['processId', 'rootPath'],
      [ ProcessId,   RootPath ]
   ),
   lsp_set_variables([processId:ProcessId, rootPath:RootPath]),
   lsp_read_project(RootPath),
   Response = json([
      id=Id,
      result=json([
         capabilities=json([
            textDocumentSync=1,
            completionProvider=json([
               resolveProvider= @true,
               triggerCharacters=[]
            ])
         ])
      ])
   ]).


lsp_handle_request('textDocument/didOpen', _, Params, _) :-
   lsp_get_from_json(Params,
      ['textDocument.uri', 'textDocument.text'],
      [ Uri,                Text]),
   lsp_add_to_list([openDocuments:Uri]),
   lsp_open_file(Uri, Text).

lsp_handle_request('textDocument/didChange', _, Params, _) :-
   lsp_get_from_json(Params,
      ['textDocument.uri', 'contentChanges'],
      [ Uri,                Changes]
   ),
   %lsp_log(debug, ['Apply changes: ', Changes]),
   forall(member(Change, Changes),
      lsp_apply_change(Uri, Change)).

lsp_apply_change(Uri, Change) :-
    %lsp_log(debug, ['Apply change: ', Change]),
   lsp_get_from_json(Change,
      ['text'],
      [ Text ]
   ),
   %lsp_log(debug, ['Change ', Uri, ' content to ', Text]),
   lsp_change_file(Uri, Text).

lsp_handle_request('textDocument/completion', Id, Params, Response) :-
   lsp_get_from_json(Params,
      ['textDocument.uri', 'position.line', 'position.character'],
      [ Uri,                Line,            Character]
   ),
   lsp_log(debug, ['Call autocomplete']),
   lsp_autocomplete(Uri, Line, Character, CompletionList),
   Response = json([
      id=Id,
      result=CompletionList
   ]).

lsp_handle_request('shutdown', Id, _, Response) :-
   Response = json([
      id=Id,
      result=json([])
   ]).

lsp_handle_request('exit', _, _, _) :-
   fail.

