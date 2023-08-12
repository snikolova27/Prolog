% list_length(Length, List) 
list_length(0, []).
list_length(Len, [ _ | T]) :- list_length(Temp, T), Len is Temp + 1.

% append(List1, List2, Result)
append([], L, L).
append([ H | T ], L , [ H | R ]):- append(T,L,R).

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