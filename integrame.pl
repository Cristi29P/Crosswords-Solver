:- ensure_loaded('checker.pl').

%test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

extract((Pos, [A]), [(Pos, A)]).
extract((Pos, [A, B]), [(Pos, A), (Pos, B)]).


intrebari(integ(_,_, [],_ ), []).
intrebari(integ(_, _,[(Pos, Val)|T], _), M) :-
        is_list(Val), extract((Pos, Val), N), append(N, Lista_intrebari, M), intrebari(integ(_, _, T, _), Lista_intrebari).
intrebari(integ(_, _,[(_, Val)|T], _), Lista_intrebari) :-
       \+is_list(Val), intrebari(integ(_, _, T, _), Lista_intrebari).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

generate_texts([],[]).
generate_texts([(_, Text, _, _)|T], [Text|L]) :- generate_texts(T, L).

id_intrebare(integ(_, _, Lista, _), Intrebare, ID) :- intrebari(integ(_, _, Lista, _), L), generate_texts(L, P), nth0(ID, P, Intrebare).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).

info_intrebare(integ(_, _, Lista, _), Intrebare, Pos, Dir) :- 
        intrebari(integ(_, _, Lista, _), L), 
        id_intrebare(integ(_, _, Lista, _), Intrebare, ID), 
        nth0(ID, L, (Pos, _, Dir, _)).

word_list([], _, _, []).
word_list([Char|List], (X, Y), Dir, [((NextX, Y), Char)|Result]) :-
        Dir == j,
        NextX is X + 1,
        word_list(List, (NextX, Y), Dir, Result).

word_list([Char|List], (X, Y), Dir, [((X, NextY), Char)|Result]) :-
        Dir == d,
        NextY is Y + 1,
        word_list(List, (X, NextY), Dir, Result).

append_result(integ(H, W, Lista, Vocab), WordList, integ(H, W, ResultedList, Vocab)) :- append(Lista, WordList, ResultedList).

completare(Integ, [], Integ).
completare(Integ, [(Q, A)|T], Integrama) :- 
        info_intrebare(Integ, Q, Pos, Dir),
        atom_chars(A, CharList), 
        word_list(CharList, Pos, Dir, StringList),
        append_result(Integ, StringList, integ(H, W, List, Vocab)),
        list_to_set(List, NoDuplicates),
        completare(integ(H, W, NoDuplicates, Vocab), T, Integrama).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).


extract_list_coords([], []).
extract_list_coords([(Coords, _)|Rest], [Coords|Result]) :- extract_list_coords(Rest, Result).

extract_column(_, [],[]).
extract_column(Column, [(X, Y)|T], [(X, Y)|T1]) :- Y == Column, extract_column(Column, T, T1).
extract_column(Column, [(_, Y)|T], T1) :- \+ (Y == Column), extract_column(Column, T, T1).

extract_line(_, [],[]).
extract_line(Line, [(X, Y)|T], [(X, Y)|T1]) :- X == Line, extract_line(Line, T, T1).
extract_line(Line, [(X, _)|T], T1) :- \+ (X == Line), extract_line(Line, T, T1).


calculate_distance_column(List_aux, (X, Y), Distance) :-
                sort(List_aux, List),
                nth0(Index, List, (X, Y)),
                Index2 is Index + 1,
                nth0(Index2, List, (X1, _)),
                Distance is X1 - X - 1.

calculate_distance_line(List_aux, (X, Y), Distance) :-
                sort(List_aux, List),
                nth0(Index, List, (X, Y)),
                Index2 is Index + 1,
                nth0(Index2, List, (_, Y1)),
                Distance is Y1 - Y - 1.

get_distance(integ(_, _, Lista, _), (X, Y), Dir, Lungime) :- 
          Dir == j,
          extract_list_coords(Lista, Lista_coords),
          extract_column(Y, Lista_coords, Lista_column),
          calculate_distance_column(Lista_column, (X, Y), Lungime).

get_distance(integ(_, _, Lista, _), (X, Y), Dir, Lungime) :- 
          Dir == d,
          extract_list_coords(Lista, Lista_coords),
          extract_line(X, Lista_coords, Lista_line),
          calculate_distance_line(Lista_line, (X, Y), Lungime).

lungime_spatiu(Integ, Intrebare, Lungime) :- info_intrebare(Integ, Intrebare, Pos, Dir), get_distance(Integ, Pos, Dir, Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).

get_pair_point(List_aux, (X, Y), (X1, Y1)) :-
                sort(List_aux, List),
                nth0(Index, List, (X, Y)),
                Index2 is Index + 1,
                nth0(Index2, List, (X1, Y1)).

helper_col(integ(_, _, Lista, _), (_, Y), Lista_column) :- 
          extract_list_coords(Lista, Lista_coords),
          extract_column(Y, Lista_coords, Lista_column).

helper_line(integ(_, _, Lista, _), (X, _), Lista_line) :- 
          extract_list_coords(Lista, Lista_coords),
          extract_line(X, Lista_coords, Lista_line).

extract_position_list(Integ, (X, Y), Dir, PosList) :- Dir == d, !, helper_line(Integ, (X, Y), PosList).

extract_position_list(Integ, (X, Y), _, PosList) :- helper_col(Integ, (X, Y), PosList).

generate_linear_line(I, I, [I]).
generate_linear_line((X, Y), (X1, Y1), [(X, Y)|Rest]) :- 
                Y < Y1,
                Y2 is Y + 1,
                generate_linear_line((X, Y2), (X1, Y1), Rest).  

generate_linear_column(I, I, [I]).
generate_linear_column((X, Y), (X1, Y1), [(X, Y)|Rest]) :- 
                X < X1,
                X2 is X + 1,
                generate_linear_column((X2, Y), (X1, Y1), Rest). 

generate_elements(Pos1, Pos2, Dir, Tail) :- Dir == d, !, generate_linear_line(Pos1, Pos2, [_|Tail]).

generate_elements(Pos1, Pos2, _, Tail) :- generate_linear_column(Pos1, Pos2, [_|Tail]).

adjust((X, Y), Dir, (X, NewY)) :- Dir == d, !, NewY is Y - 1.
adjust((X, Y), _, (NewX, Y)) :- NewX is X - 1.

intersectie(integ(H, W, Lista, Voc), I1, Poz1, I2, Poz2) :- 
        info_intrebare(integ(H, W, Lista, Voc), I1, Pos1, Dir1),
        info_intrebare(integ(H, W, Lista, Voc), I2, Pos2, Dir2),
        I1 \= I2,
        extract_position_list(integ(H, W, Lista, Voc), Pos1, Dir1, PosListI1),
        extract_position_list(integ(H, W, Lista, Voc), Pos2, Dir2, PosListI2),
        get_pair_point(PosListI1, Pos1, Pos1_aux1),
        get_pair_point(PosListI2, Pos2, Pos2_aux1),
        adjust(Pos1_aux1, Dir1, Pos1_aux),
        adjust(Pos2_aux1, Dir2, Pos2_aux),
        generate_elements(Pos1, Pos1_aux, Dir1, Resulting1),
        generate_elements(Pos2, Pos2_aux, Dir2, Resulting2),
        intersection(Resulting1, Resulting2, [(A, B)|_]),
        nth0(Poz1, Resulting1, (A, B)),
        nth0(Poz2, Resulting2, (A, B)).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
create_tokens_list([], []).
create_tokens_list([H|T], [H1|Rest]) :- atom_chars(H, H1), create_tokens_list(T, Rest).

generate_fitting_list(_,[],[]).
generate_fitting_list(Lungime, [H|T], [H|T1]) :- length(H, Len), Len == Lungime, generate_fitting_list(Lungime, T, T1).
generate_fitting_list(Lungime, [H|T], T1) :- length(H, Len), \+(Len == Lungime), generate_fitting_list(Lungime, T, T1).

find_matching(Integ, Tokens, Intrebare, List) :- lungime_spatiu(Integ, Intrebare, Lungime), generate_fitting_list(Lungime, Tokens, List).

extract_questions(integ(_, _, Lista, _), P) :- intrebari(integ(_, _, Lista, _), L), generate_texts(L, P).

create_pairings(_,_ ,[], []).
create_pairings(Integ, Tokens, [Q|T], [(Q, Rsp)|Rest]) :- 
                        find_matching(Integ, Tokens, Q, Rsp),
                        create_pairings(Integ, Tokens, T, Rest).

solutii_posibile(integ(H, W, Lista, Vocabular), Solutii) :- 
                        extract_questions(integ(H, W, Lista, Vocabular), Q), 
                        create_tokens_list(Vocabular, Tokens),
                        create_pairings(integ(H, W, Lista, Vocabular), Tokens, Q, Solutii).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.

rezolvare(Integ, _) :-
        solutii_posibile(Integ, _).


% final--- lungime-spatiu, intersectie, solutii-posibile, exclude, include, findall, bagof, check_intersection

% iau fiercare cuvant, ma asigur ca se intersecteaza corect cu toate celelalte
% adaug un cuvant la solutie, de fiecare data cand adaug, verific ca intersectia(daca exista) sa aiba aceleasi litere

% pt a apela functia asta, se verifica inainte ca ele se intersecteaza;
% functia verifica ca se intersecteaza in aceeasi litera
check_intersection(W1, P1, W2, P2) :-
        nth0(P1, W1, C1),
        nth0(P2, W2, C2),
        C1 == C2.


