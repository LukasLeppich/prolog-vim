:- module(lsp_json_utils, [lsp_get_from_json/3]).
/** <module> A helper module to handle json objects

This module contains some utility functions to handle json objects.

@author Lukas Leppich <lukas.leppich@gmail.com>
*/

/**
 * lsp_get_from_json(+Json:json, +Paths:list, -Values:list)
 *
 * Read the provided paths from the json object.
 *
 * Example:
 *
 * ~~~{.pl}
 * JSON = json([name='Demo term',
 *              created=json([day='30', month='2', year='1712'])])
 * lsp_get_from_json(JSON,
 *    ['name', 'created.year'],
 *    [Name,   Year])
 * ~~~
 *
 * @arg Json The json object.
 * @arg Paths A list of json paths.
 * @arg Values The values from the json object.
 */
lsp_get_from_json(_, [], []).

lsp_get_from_json(Json, [Path|Paths], [Value|Values]) :-
    get_single_from_json(Json, Path, Value),
    lsp_get_from_json(Json, Paths, Values).

/**
 * get_single_from_json(+Json:json, +Path:string, -Value:string)
 * 
 * Get a single path from the json object.
 *
 * @arg Json The json object.
 * @arg Path The json path.
 * @arg Value The value at the provided json path.
 */
get_single_from_json(Json, Path, Value) :-
   \+ is_list(Path),
   atomic_list_concat(Paths, '.', Path),
   get_from_json_path(Json, Paths, Value).

/**
 * get_from_json_path(+Json:json, +Path:list, -Value:string)
 *
 * Follow the path list and return the value at the destination.
 *
 * @arg Json The json object.
 * @arg Path The json path as a list.
 * @arg Value The value at the provided json path.
 */
get_from_json_path(Json, [P], Value) :-
   Json=json(List),
   string_to_atom(P, PAtom),
   member(PAtom=Value, List).

get_from_json_path(Json, [P|Ps], Value) :-
   Json=json(List),
   string_to_atom(P, PAtom),
   member(PAtom=X, List),
   get_from_json_path(X, Ps, Value).
   
