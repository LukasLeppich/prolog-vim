:- begin_tests(lsp_json_utils).
:- use_module(lsp_json_utils).

test(lsp_get_from_json) :-
    Json = json([name='Demo term',
                created=json([day='30', month='2', year='1712'])
                ]),
    lsp_get_from_json(Json,
        ['name', 'created.year'],
        ['Demo term', '1712']).
    
test(lsp_get_from_json_not_found, [fail]) :-
    Json = json([name='Demo term',
                created=json([day='30', month='2', year='1712'])
                ]),
    lsp_get_from_json(Json,
        ['not_found'],
        [_]).

test(lsp_get_from_json_empty_list) :-
    Json = json([name='Demo term',
                created=json([day='30', month='2', year='1712'])
                ]),
    lsp_get_from_json(Json, [], []).


test(lsp_get_from_json_non_equal_size_1, [fail]) :-
    Json = json([name='Demo term',
                created=json([day='30', month='2', year='1712'])
                ]),
    lsp_get_from_json(Json, 
      ['name'],
      ['Demo term', _]).

test(lsp_get_from_json_non_equal_size_2, [fail]) :-
    Json = json([name='Demo term',
                created=json([day='30', month='2', year='1712'])
                ]),
    lsp_get_from_json(Json, 
      ['name', 'created.day'],
      ['Demo term']).

:- end_tests(lsp_json_utils).
