:- module(lsp_storage, [
      lsp_set_variables/1,
      lsp_get_variables/1,
      lsp_remove_variables/1,
      lsp_add_to_list/1,
      lsp_remove_from_list/1,
      lsp_remove_all/0
   ]).

/** <module> Helper module to save information in memory.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

:- dynamic
   lsp_variable/2.

/**
 * lsp_get_variables(+Variables:list)
 *
 * Load the provides list of key:value pairs from memory.
 *
 * Usage:
 * 
 * ~~~{.pl}
 * lsp_get_variables([logging_file:File]).
 * ~~~
 *
 * @arg Variables A list of key:value pairs.
 */
lsp_get_variables([]).
lsp_get_variables([Name:Value|Variables]) :-
    lsp_variable(Name, Value),
    lsp_get_variables(Variables).

/**
 * lsp_set_variables(+Variables:list)
 *
 * Save the provides list of key:value pairs in memory.
 *
 * Usage:
 * 
 * ~~~{.pl}
 * lsp_set_variables([logging_file:file]).
 * ~~~
 *
 * @arg Variables A list of key:value pairs.
 */
lsp_set_variables([]).
lsp_set_variables([Name:Value|Variables]) :-
   lsp_set_variable(Name, Value),
   lsp_set_variables(Variables).

/**
 * lsp_get_variable(+Name:atom, -Value:atom)
 *
 * Get the value of the specified variable name.
 *
 * @arg Name The name of the variable.
 * @arg Value The value of the variable.
 */
lsp_get_variable(Name, Value) :-
   lsp_variable(Name, Value).

/**
 * lsp_set_variable(+Name:atom, +Value:atom)
 *
 * Save the variable in memory.
 *
 * @arg Name The name of the variable.
 * @arg Value The value of the variable.
 */
lsp_set_variable(Name, Value) :-
   retractall(lsp_variable(Name, _)),
   assert(lsp_variable(Name, Value)).

/**
 * lsp_remove_variables(+Names:list)
 *
 * Remove the specified variable names from memory.
 *
 * @arg Names A list of names to remove.
 */
lsp_remove_variables([]).
lsp_remove_variables([Name|Names]) :-
    retractall(lsp_variable(Name, _)),
    lsp_remove_variables(Names).

/**
 * lsp_add_to_list(+Variables:list)
 *
 * For each key:value pair, add value to the list specified by key.
 *
 * @arg Variables The key:value pair list.
 */
lsp_add_to_list([]).
lsp_add_to_list([Name:Value|Variables]) :-
   ( lsp_get_variable(Name, OldValue) ->
     append(OldValue, [Value], NewValues)
   ; NewValues = [Value] ),
   lsp_set_variable(Name, NewValues),
   lsp_add_to_list(Variables).

/**
 * lsp_remove_from_list(+Variables:list)
 *
 * For each key:value pair, remove value from the list specified by key.
 *
 * @arg Variables The key:value pair list.
 */
lsp_remove_from_list([]).
lsp_remove_from_list([Name:Value|Variables]) :-
   ( lsp_get_variable(Name, OldValue) ->
     delete(OldValue, Value, NewValues),
     lsp_set_variable(Name, NewValues)
   ; true),
   lsp_remove_from_list(Variables).


/**
 * lsp_remove_all
 *
 * Delete all saved values.
 *
 */
lsp_remove_all :-
    retractall(lsp_variable(_, _)).


