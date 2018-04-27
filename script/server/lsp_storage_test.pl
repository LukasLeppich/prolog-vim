:- begin_tests(lsp_storage).
:- use_module(lsp_storage).

clean_storage :-
    lsp_remove_all.

test(lsp_set_variables, [cleanup(clean_storage)]) :-
    Variables=[key:test_value],
    lsp_set_variables(Variables),
    lsp_get_variables([key:StoredValue]),
    StoredValue == test_value.

test(lsp_set_multiple_variables, [cleanup(clean_storage)]) :-
    Variables=[key1:test_value_1, key2:test_value_2, key3:[1,2,3]],
    lsp_set_variables(Variables),
    lsp_get_variables([key1:test_value_1]),
    lsp_get_variables([key2:test_value_2]),
    lsp_get_variables([key3:[1,2,3]]).

test(lsp_remove_variables, [cleanup(clean_storage)]) :-
    Variables=[key1:test_value_1, key2:test_value_2],
    lsp_set_variables(Variables),
    lsp_get_variables([key1:test_value_1]),
    lsp_remove_variables([key1]),
    lsp_get_variables([key2:test_value_2]),
    \+ lsp_get_variables([key1:_]).

test(lsp_add_to_list, [cleanup(clean_storage)]) :-
    Variables=[key1:k1v1,key1:k1v2,key2:k2v1],
    lsp_add_to_list(Variables),
    lsp_get_variables([key1:List1]),
    length(List1, 2),
    member(k1v1, List1),
    member(k1v2, List1),
    lsp_get_variables([key2:List2]),
    length(List2, 1),
    member(k2v1, List2),
    lsp_add_to_list([key2:k2v2]),
    lsp_get_variables([key2:List3]),
    length(List3, 2),
    member(k2v1, List3),
    member(k2v2, List3).

test(lsp_remove_from_list, [cleanup(clean_storage)]) :-
    Variables=[key1:k1v1,key1:k1v2,key2:k2v1],
    lsp_add_to_list(Variables),
    lsp_get_variables([key1:List1]),
    length(List1, 2),
    member(k1v1, List1),
    member(k1v2, List1),
    lsp_get_variables([key2:List2]),
    length(List2, 1),
    member(k2v1, List2),
    lsp_remove_from_list([key1:k1v1, key2:k2v1]),
    lsp_get_variables([key1:List1_2]),
    lsp_get_variables([key2:List2_2]),
    length(List1_2, 1),
    member(k1v2, List1_2),
    length(List2_2, 0).

