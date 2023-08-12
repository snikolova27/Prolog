% list_length(Length, List) 
list_length(0, []).
list_length(Len, [ _ | T]) :- list_length(Temp, T), Len is Temp + 1.

% append(List1, List2, Result)
append([], L, L).
append([ H | T ], L , [ H | R ]):- append(T,L,R).

% last(Elem, List) -> Elem is the last element of List
last(Elem, {Elem}).
last(Elem, [ _ | Tail]) :- last(Elem, Tail).

% reverse(L, Reversed)
reverse([],[]).
reverse( [ H | T ] , Result):- reverse(T, TempResult), append(TempResult,[H], Result).

% member(Elem, List) -> is Elem member of List
member_sonik(Elem, [ Elem | _ ]).
member_sonik(Elem, [ H | Tail ]):-member(Elem,Tail), H \= Elem.

% distinct(List, Distinct) -> generates the distinct elements of List in Distinct
distinct([],[]).
distinct([ H | T ],  [ H | R ]) :- distinct(T,R), not(member_sonik(H,R)).
distinct( [H | T], R) :- distinct(T,R), member_sonik(H,R).

% suffix(Prefix, List) 
prefix(Prefix, L) :- append(Prefix, _ , L).

% suffix(Suffix, List)
suffix(Suffix, L) :- append( _ , Suffix, L).


% insert(Elem, List, Result)
insert(Elem, List, Result) :- append(Prefix, Suffix, List), append(Prefix, [ Elem | Suffix ], Result).

% permutate(Permutation, List)
permutate([], []).
permutate(Permutation, [ H | T ]):- permutate(TempPerm, T), insert(H, TempPerm, Permutation).


% subsequence(Subsequnce, List)
subsequence([],[]).
% choose to include the current element
subsequence(Subseq, [ H | T ]) :- subsequence(SubseqTemp, T), insert(H, SubseqTemp, Subseq).
% do not include current element
subsequence(Subseq, [ _ | T ]) :- subsequence(Subseq, T).

% order (A, B) -> checks if A <= B
order(A,B) :- A =< B.

% is_sorted(List) -> checks if a list is sorted in some order
is_sorted([]).
is_sorted([ _]).
is_sorted([A , B | T]) :- order(A, B), is_sorted([B|T]).

% bogo_sort(Sorted, List) -> Sorted is the sorted version of List
bogo_sort( Permutation, List) :- permutate(Permutation, List), is_sorted(Permutation).

% between_sonik(Elem, LowerBound, UpperBound) -> generates a number in the interval [LowerBound, UpperBound]
between_sonik(LowerBound, LowerBound, UpperBound) :- LowerBound =< UpperBound.
between_sonik(Elem, LowerBound, UpperBound) :-
                             LowerBound < UpperBound,
                             NewLowerBound is LowerBound + 1,
                             between_sonik(Elem, NewLowerBound, UpperBound).
                            
% range_sonik(Range, LowerBound, UpperBound) -> generates a list Range with all the numbers in [LowerBound, UpperBound]                            
range_sonik([], LowerBound, UpperBound) :- UpperBound < LowerBound.
range_sonik( [LowerBound | T], LowerBound, UpperBound) :- LowerBound =< UpperBound, 
                    NewLowerBound is LowerBound + 1,
                    range_sonik(T, NewLowerBound, UpperBound).
                
% nat(N) -> checks if N is a natural number
nat(0).
nat(N) :- nat(K), N is K + 1.

% gen_KS(Len, Sum, List) -> generates a list with length Len and Sum of its members equal to Sum
gen_KS(1,Sum, [Sum]).
gen_KS(Len, Sum, [Elem | R]) :-
    Len > 1,
    between_sonik(Elem, 0, Sum),
    NewSum is Sum - Elem,
    NewLen is Len - 1,
    gen_KS(NewLen, NewSum, R).

% gen_nat_pair(A,B) -> generates a pair of natural numbers
gen_nat_pair(A, B):-
    nat(Sum),
    gen_KS(2, Sum, [ A | B ])
    
% int(N,Z) -> generate Z belonging to the whole numbers set
int(0, 0).
int(N, N) :- N > 0.
int(N, Z) :- N > 0, Z is -N.

% gen_arithmethic_progression(List)
gen_arithmethic_progression([]).
gen_arithmethic_progression(Progression) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L > 0, % length 
    int(K, A0),
    int(M, D), % step / difference between elements
    gen_arithmethic_progression(L, A0, D, Progression).

gen_arithmethic_progression(1, A0, 0, [A0]).
gen_arithmethic_progression(L, A0, D, Progression) :-
    L > 1,
    gen_ar_prog_with_rec(L, A0, D, Progression).

gen_ar_prog_with_rec(1, A0, _, [A0]).
gen_ar_prog_with_rec(L, A0, D, [A0 | Progression]) :-
    L > 1,
    L1 is L - 1,
    A1 is A0 + D,
    gen_ar_prog_with_rec(L1, A1, D, Progression).