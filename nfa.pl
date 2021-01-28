%%%% Pietro Epis 845045
%%%% Michele Milesi 844682

%%%% -*- Mode: Prolog -*-

%%%% nfa.pl --
%%%%
%%%% Progetto Linguaggi di Programmazione

%%%% is_regexp/1
%%%% is_regexp(RE) is true if RE represents a valid Regular Expression

is_regexp(RE) :- atomic(RE), !.
is_regexp(RE) :-
    compound(RE),
    RE =.. [Op | _],
    Op \= seq,
    Op \= or,
    Op \= plus,
    Op \= star,
    !.
is_regexp(star(RE)) :- !, is_regexp(RE).
is_regexp(plus(RE)) :- !, is_regexp(RE).

is_regexp(RE) :- functor(RE, or, _),
    RE =.. [or, A | As], !, is_regexp(A),
    RE1 =.. [or | As], is_regexp(RE1).

is_regexp(RE) :- functor(RE, seq, _),
    RE =.. [seq, A | As], !, is_regexp(A),
    RE1 =.. [seq | As], is_regexp(RE1).

%%%% nfa_regexp_comp/2
%%%% nfa_regexp_comp(FA_Id, RE) checks whether RE is a valid
%%%% Regular Expression, creates initial and final states and
%%%% then calls build_nfa

nfa_regexp_comp(FA_Id, RE) :-
    is_regexp(RE),
    nonvar(FA_Id),
    gensym(q, InitialState),
    gensym(q, FinalState),
    assert(nfa_initial(FA_Id, InitialState)),
    assert(nfa_final(FA_Id, FinalState)),
    RE =.. L,
    build_nfa(FA_Id, L, InitialState, FinalState).

%%%% nfa_test/2
%%%% nfa_test(FA_Id, Input) checks whether the automata referenced by
%%%% FA_Id accepts the Input or not

nfa_test(FA_Id, Input) :-
    nfa_initial(FA_Id, InitialState),
    nfa_accept(FA_Id, Input, InitialState).

%%%% nfa_accept/3
%%%% nfa_accept(FA_Id, Input, Q) traverses the automata referenced by
%%%% FA_Id in relation to Input.

nfa_accept(FA_Id, Input, CurrentState) :-
   nfa_delta(FA_Id, CurrentState, [], NextState),
   nfa_accept(FA_Id, Input, NextState).

nfa_accept(FA_Id, [ I | Is], CurrentState) :-
   nfa_delta(FA_Id, CurrentState, I, NextState),
   nfa_accept(FA_Id, Is, NextState).

nfa_accept(FA_Id, [], CurrentState) :- nfa_final(FA_Id, CurrentState).

%%%% build_nfa/4
%%%% build_nfa(FA_Id, RE, X, Y) actually creates states and
%%%% transitions of the automata

build_nfa(FA_Id, [L1], InitialState, FinalState) :-
    atomic(L1),
    L1 \= or,
    L1 \= seq,
    L1 \= epsilon,
    assert(nfa_delta(FA_Id, InitialState, L1, FinalState)).

build_nfa(FA_Id, [epsilon], InitialState, FinalState) :-
    assert(nfa_delta(FA_Id, InitialState, [], FinalState)).

build_nfa(FA_Id, [L1, L2 | Ls], InitialState, FinalState) :-
    L1 = or,
    gensym(q, SubInitState),
    gensym(q, SubFinalState),
    L2 =.. L2List,
    build_nfa(FA_Id, L2List, SubInitState, SubFinalState),
    build_nfa(FA_Id, [L1 | Ls], InitialState, FinalState),
    assert(nfa_delta(FA_Id, InitialState, [], SubInitState)),
    assert(nfa_delta(FA_Id, SubFinalState, [], FinalState)).

build_nfa(_, [L1], _, _) :-
    L1 = or,
    !.

build_nfa(FA_Id, [L1, L2 | Ls], InitialState, FinalState) :-
    L1 = seq,
    gensym(q, SubInitState),
    gensym(q, SubFinalState),
    L2 =.. L2List,
    build_nfa(FA_Id, L2List, SubInitState, SubFinalState),
    build_nfa(FA_Id, [L1 | Ls], SubFinalState, FinalState),
    assert(nfa_delta(FA_Id, InitialState, [], SubInitState)).

build_nfa(FA_Id, [L1], InitialState, FinalState) :-
    L1 = seq,
    assert(nfa_delta(FA_Id, InitialState, [], FinalState)).

build_nfa(FA_Id, [L1, L2], InitialState, FinalState) :-
    L1 = star,
    gensym(q, SubInitState),
    gensym(q, SubFinalState),
    L2 =.. L2List,
    build_nfa(FA_Id, L2List, SubInitState, SubFinalState),
    assert(nfa_delta(FA_Id, InitialState, [], SubInitState)),
    assert(nfa_delta(FA_Id, SubFinalState, [], FinalState)),
    assert(nfa_delta(FA_Id, SubFinalState, [], SubInitState)),
    assert(nfa_delta(FA_Id, InitialState, [], FinalState)).

build_nfa(FA_Id, [L1, L2], InitialState, FinalState) :-
    L1 = plus,
    build_nfa(FA_Id, [seq, L2, star(L2)], InitialState, FinalState).

%%%% nfa_clear/0
%%%% nfa_clear() removes all previously compiled automata by calling
%%%% nfa_clear(FA_Id)

nfa_clear() :- nfa_clear(_).

%%%% nfa_clear/1
%%%% nfa_clear(FA_Id) removes the automata referenced by the FA_Id

nfa_clear(FA_Id) :-
    retractall(nfa_initial(FA_Id, _)),
    retractall(nfa_final(FA_Id, _)),
    retractall(nfa_delta(FA_Id, _, _, _)).

%%%% nfa_clear/0
%%%% nfa_clear() shows all previously complied automata by calling
%%%% nfa_list(FA_Id)

nfa_list() :- nfa_list(_).

%%%% nfa_list/1
%%%% nfa_list(FA_Id) shows the automata referenced by the FA_Id

nfa_list(FA_Id) :-
    listing(nfa_initial(FA_Id, _)),
    listing(nfa_final(FA_Id, _)),
    listing(nfa_delta(FA_Id, _, _, _)).

%%%% end of file -- nfa.pl --
