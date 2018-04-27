:- module(lsp_tokens, [
   lsp_read_project/1,
   lsp_open_file/2,
   lsp_change_file/2,
   lsp_get_tokens_for_file/2,
   lsp_get_all_tokens/1
]).

:- use_module(library(memfile)).
:- use_module(library(prolog_source)).

:- use_module(lsp_logging).
:- use_module(lsp_storage).

/** <module> Search for tokens in files.
 
Provide functions to extract tokens from prolog files.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_read_project(+RootPath:string)
 * 
 * Parse all prolog files of the provided root path and its childs.
 *
 * @arg RootPath The file path of the root directory.
 */
lsp_read_project(RootPath) :-
   directory_files(RootPath, Entries),
   lsp_log(debug, ['Check entries of ' , RootPath]),
   %ErrorStream = user_error,
   %OutputStream = user_output,
   %lsp_get_log_stream(LogStream),
   lsp_get_log_file(LogFile),
   telling(OriginalOutput),
   tell(LogFile),
   %set_prolog_IO(current_input, LogStream, LogStream),
   lsp_read_files(RootPath, Entries),
   tell(OriginalOutput).
   %set_prolog_IO(current_input, OutputStream, ErrorStream),
   %close(LogStream),
   %set_output(OutputStream).

/**
 * lsp_read_files(+RootPath:string, +Entries:list)
 *
 * Parse all provided prolog entities, recursively search in sub directories.
 *
 * @arg RootPath The current directory.
 * @arg Entries A list of files inside of the current directory.
 */
lsp_read_files(_, []).

lsp_read_files(RootPath, [Entry | Entries]) :-
   directory_file_path(RootPath, Entry, Path),
   exists_directory(Path),
   Entry \== '.',
   Entry \== '..',
   lsp_read_files(RootPath, Entries),
   lsp_log(debug, ['Is directory, search for files in ', Path]),
   lsp_read_project(Path).

lsp_read_files(RootPath, [Entry | Entries]) :-
   \+ file_name_extension(_, pl, Entry),
   lsp_log(debug, ['No pl extension: ', Entry]),
   lsp_read_files(RootPath, Entries).

lsp_read_files(RootPath, [Entry | Entries]) :-
   directory_file_path(RootPath, Entry, Path),
   lsp_log(debug, ['Read file content: ', Path]),
   lsp_read_file(Path),
   lsp_read_files(RootPath, Entries).


/**
 * lsp_read_file(+File:string)
 *
 * Read all terms of the provided file path and add them to the storage.
 *
 * @arg File The file path.
 */
lsp_read_file(File) :-
   open(File, read, Stream),
   lsp_read_all_terms(Stream, Tokens),
   close(Stream),
   lsp_add_tokens(File, Tokens).


/**
 * lsp_open_file(+Uri:string, +Content:string)
 *
 * Add tokens of a new file to the token storage.
 *
 * @arg Uri The URI to the newly opened file.
 * @arg Content The content of the file.
 */
lsp_open_file(Uri, Content) :-
   lsp_change_file(Uri, Content).

/**
 * lsp_change_file(+Uri:string, +Content:string)
 *
 * Remove all old tokens and read the new file content.
 *
 * @arg Uri The URI to the file that has changed.
 * @arg Content The content of the file.
 */
lsp_change_file(Uri, Content) :-
   lsp_remove_tokens(Uri),
   lsp_set_variables([Uri:Content]),
   atom_to_memory_file(Content, Handler),
   lsp_read_tokens(Handler, Tokens),
   lsp_add_tokens(Uri, Tokens).

/**
 * lsp_get_all_tokens(-Tokens:list)
 *
 * Get a list of all saved tokens.
 *
 * @arg Tokens A list of all tokens.
 */
lsp_get_all_tokens(Tokens) :-
   lsp_get_variables([token_urls:TokenUrls]),
   lsp_log(debug, ['Find tokens for urls: ', TokenUrls]),
   findall(Token, (
      member(TokenUrl, TokenUrls),
      lsp_log(debug, ['get tokens for ', TokenUrl]),
      lsp_get_tokens_for_token_uri(TokenUrl, Token),
      lsp_log(debug, ['Tokens found: ', Token])
   ), FoundTokens),
   append(FoundTokens, Tokens).

/**
 * lsp_get_tokens_for_file(+Uri:string, -Tokens:list)
 *
 * Get all tokens for the specified file.
 *
 * @arg Uri The Uri of the file.
 * @arg Tokens A list of tokens that were read from the file.
 */
lsp_get_tokens_for_file(Uri, Tokens) :-
   lsp_get_token_uri(Uri, TokenUri),
   lsp_get_tokens_for_token_uri(TokenUri, Tokens).

/** 
 * lsp_get_tokens_for_token_uri(+TokenUri:string, -Tokens:list)
 *
 * Get all tokens for the specified tokens url.
 *
 * @arg TokenUri The token uri.
 * @arg Tokens The tokens saved at the token uri.
 */
lsp_get_tokens_for_token_uri(TokenUri, Tokens) :-
   ( lsp_get_variables([TokenUri:Tokens]) -> true
   ; Tokens = []).

/**
 * lsp_read_tokens(+Handler:handle, -Tokens:list)
 *
 * Read all tokens from the file handle.
 *
 * @arg Handler The file handle to read from.
 * @arg Tokens The list of tokens read from the handle.
 */
lsp_read_tokens(Handler, Tokens) :-
   open_memory_file(Handler, read, Stream),
   lsp_read_all_terms(Stream, Tokens),
   close(Stream).

/**
 * lsp_read_all_terms(+Stream:stream, -Tokens:list)
 *
 * Read all tokens from the stream.
 *
 * @arg Stream The input stream to read tokens from.
 * @arg Tokens The tokens read from the stream.
 */
lsp_read_all_terms(Stream, Tokens) :-
   prolog_read_source_term(Stream, Term, Expand, [
      % dec10 => print and continue
      syntax_errors(dec10),
      comments(Comment),
      variable_names(Names),
      term_position(Position)
   ]),
   ( Term == end_of_file -> (
      Tokens = [],
      lsp_log(debug, ['no more tokens'])
   ) ; (
      %lsp_log(debug, ['Token found: ', Term]),
      lsp_read_all_terms(Stream, NextTokens),
      Tokens = [[Term, Expand, Comment, Names, Position] | NextTokens]
   )).

/**
 * lsp_add_tokens(+Uri:string, +Tokens:list)
 *
 * Add tokens to the storage using the provided uri.
 *
 * @arg Uri The token uri to store the tokens.
 * @arg Tokens The tokens to store.
 */
lsp_add_tokens(Uri, Tokens) :-
   lsp_get_token_uri(Uri, TokenUri),
   lsp_add_to_list([token_urls:TokenUri]),
   %lsp_log(debug, ['at tokens to uri: ', TokenUri]),
   forall(member([Token|_], Tokens),
      lsp_add_token(TokenUri, Token)).
    
/**
 * lsp_add_token(+TokenUri:string, Token:predicate)
 *
 * Add token to the token list saved at the specified token uri.
 *
 * @arg TokenUri The token uri to store the token.
 * @arg Token The token to store.
 */
lsp_add_token(TokenUri, Token) :-
    %lsp_log(debug, ['token: ', Token]),
    ( (Head:-_) = Token -> 
      %lsp_log(debug, ['Header: ', Head]),
      functor(Head, Name, _),
      %lsp_log(debug, ['Add token: ', Name]),
      lsp_add_to_list([TokenUri:Name])
     ; true).

/**
 * lsp_remove_tokens(+Uri:string)
 *
 * Remove all tokens saved at uri.
 *
 * @arg Uri The token uri to remove.
 */
lsp_remove_tokens(Uri) :-
   lsp_get_token_uri(Uri, TokenUri),
   lsp_remove_from_list([token_urls:TokenUri]),
   lsp_remove_variables([TokenUri]).

/**
 * lsp_get_token_uri(+Uri:string, -TokenUri:string)
 *
 * Generate a token uri for the specified file uri.
 *
 * @arg Uri The uri of the file.
 * @arg TokenUri The token uri.
 */
lsp_get_token_uri(Uri, TokenUri) :-
   atom_concat('__tokens__', Uri, TokenUri).

