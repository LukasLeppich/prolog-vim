:- module(lsp_autocomplete, [lsp_autocomplete/4]).

:- use_module(lsp_logging).
:- use_module(lsp_storage).
:- use_module(lsp_tokens).

/** <module> Handle autocomplete requests.
 
This module can handle autocomplete requests. 

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_autocomplete(+Uri:string, +Line:integer, +Character:integer, -CompletionList:list)
 *
 * Search for autocompletions.
 *
 * @arg Uri The file uri.
 * @arg Line The current cursor line.
 * @arg Character The current cursor position.
 * @CompletionList A list with posible completions.
 */
lsp_autocomplete(Uri, Line, Character, CompletionList) :-
   lsp_get_variables([Uri:Text]),
   get_word_on_position(Text, Line, Character, Word),
   lsp_log(debug, ['Word on cursor position: ', Word]),
   % lsp_get_tokens_for_file(Uri, Tokens),
   lsp_get_all_tokens(Tokens),
   lsp_log(debug, ['Found tokens: ', Tokens]),
   lsp_filter_by_word(Tokens, Word, MatchingTokens),
   lsp_create_response(MatchingTokens, CompletionList).


lsp_filter_by_word([], _, []).
lsp_filter_by_word([I|Is], Word, Output) :-
    ( sub_atom_icasechk(I, 0, Word) -> Output = [I|Next]
    ; Output = Next 
    ),
    lsp_filter_by_word(Is, Word, Next).
        

lsp_create_response([], []).
lsp_create_response([T|Ts], [R|Rs]) :-
    R = json([
      label=T
    ]),
    lsp_create_response(Ts, Rs).

get_word_on_position(Text, LineNumber, CharacterNumber, Word) :-
   atomic_list_concat(Lines, '\n', Text),
   length(Lines, LineCount),
   ( LineCount >= LineNumber -> true
   ; lsp_log(error, ['File has only ', LineCount,
       ' lines, not ', LineNumber])
   ), 
   nth1(LineNumber, Lines, Line),
   string_to_list(Line, Characters),
   find_current_word(Characters, CharacterNumber, WordCharacters),
   string_to_list(Word, WordCharacters).

find_current_word(Characters, Number, Word) :-
   length(Characters, CLength),
   BackNumber is Number - 1,
   find_current_word_backwards(Characters, BackNumber, Backwards),
   ( CLength < Number ->
     find_current_word_forwards(Characters, Number, Forewards)
   ; Forewards = [] ),
   append(Backwards, Forewards, Word).

find_current_word_backwards(_, 0, []).

find_current_word_backwards(Characters, Number, Word) :-
	NextNumber is Number - 1,
	nth1(Number, Characters, Char),
	(( is_alnum(Char) ; Char = 95 /* 95 is '_' */) ->
     find_current_word_backwards(Characters, NextNumber, Prev),
	  append(Prev, [Char], Word)
	; Word = []).

find_current_word_forwards(Characters, Number, []) :-
   length(Characters, CLength),
   CLength =< Number.

find_current_word_forwards(Characters, Number, Word) :-
	NextNumber is Number + 1,
	nth1(Number, Characters, Char),
	(( is_alnum(Char) ; Char = 95 /* 95 is '_' */ ) ->
     find_current_word_forwards(Characters, NextNumber, Next),
     append([Char], Next, Word)
	; Word = []).

