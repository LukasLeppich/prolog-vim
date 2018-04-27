
:- initialization tests.

tests :-
    A = [lsp_json_utils_test, lsp_storage_test],
    consult(A),
    run_tests(lsp_json_utils),
    run_tests(lsp_storage),
    halt(0).
