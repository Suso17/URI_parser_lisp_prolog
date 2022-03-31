%%%% -*- Mode:prolog -*-


%%%% uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment).


uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    recognize_mailto(URIList),

    parse_scheme_mail(URIList, Acc, SchemeList, Rest0),
    get_ready_for_output(SchemeList, Scheme),
    
    parse_userinfo_mail(Rest0, Acc, UserList, Rest1),
    get_ready_for_output(UserList, Userinfo),

    parse_host_mail(Rest1, Acc, HostList, _),
    \+ last(HostList, '.'),
    get_ready_for_output(HostList, Host),

    URI = uri(
	      Scheme,
	      Userinfo,
	      Host
	      ), !.

uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    recognize_zos(URIList),

    parse_scheme_zos(URIList, Acc, SchemeList, Rest0),
    get_ready_for_output(SchemeList, Scheme),

    check_authority(Rest0, CheckAuthority),
    
    check_userinfo(Rest0, CheckAuthority, CheckUser),
    parse_userinfo(Rest0, Acc, UserList, Rest1, CheckUser),
    get_ready_for_output(UserList, Userinfo),

    parse_host(Rest1, Acc, HostList, Rest2, CheckAuthority),
    \+ last(HostList, '.'),
    get_ready_for_output(HostList, Host),

    check_port(Rest2, CheckAuthority, CheckPort),
    parse_port(Rest2, Acc, PortList, Rest3, CheckPort),
    get_ready_for_output(PortList, Port),

    parse_id44(Rest3, Acc, Id44List, Rest4),
    parse_id8(Rest4, Acc, Id8List, Rest5),
    ok_id44(Id44List),
    ok_id8(Id8List),
    append(Id44List, Id8List, PathList),
    get_ready_for_output(PathList, Path),
    
    parse_query(Rest5, Acc, QueryList, Rest6),
    get_ready_for_output(QueryList, Query),

    parse_fragment(Rest6, Acc, FragmentList, _),
    get_ready_for_output(FragmentList, Fragment),
    
    URI = uri(
	      Scheme,
	      Userinfo,
	      Host,
	      Port,
	      Path,
	      Query,
	      Fragment), !.


uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    recognize_news(URIList),

    parse_scheme_news(URIList, Acc, SchemeList, Rest0),
    get_ready_for_output(SchemeList, Scheme),

    parse_host_news(Rest0, Acc, HostList, _),
    \+ last(HostList, '.'),
    get_ready_for_output(HostList, Host),
    
    URI = uri(
	      Scheme,
	      Host
	      ), !.

uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    recognize_tel(URIList),

    parse_scheme_tel(URIList, Acc, SchemeList, Rest0),
    get_ready_for_output(SchemeList, Scheme),

    parse_userinfo_tel(Rest0, Acc, UserList, _),
    get_ready_for_output(UserList, Userinfo),
    
    URI = uri(
	      Scheme,
	      Userinfo
	      ), !.

uri_parse(URIString, URI) :-
    string_chars(URIString, URIList),
    recognize_normal_scheme(URIList),
    
    parse_scheme(URIList, Acc, SchemeList, Rest0),
    get_ready_for_output(SchemeList, Scheme),

    check_authority(Rest0, CheckAuthority),
    
    check_userinfo(Rest0, CheckAuthority, CheckUser),
    parse_userinfo(Rest0, Acc, UserList, Rest1, CheckUser),
    get_ready_for_output(UserList, Userinfo),

    parse_host(Rest1, Acc, HostList, Rest2, CheckAuthority),
    \+ last(HostList, '.'),
    get_ready_for_output(HostList, Host),

    check_port(Rest2, CheckAuthority, CheckPort),
    parse_port(Rest2, Acc, PortList, Rest3, CheckPort),
    get_ready_for_output(PortList, Port),

    parse_path(Rest3, Acc, PathList, Rest4),
    get_ready_for_output(PathList, Path),

    parse_query(Rest4, Acc, QueryList, Rest5),
    get_ready_for_output(QueryList, Query),

    parse_fragment(Rest5, Acc, FragmentList, _),
    get_ready_for_output(FragmentList, Fragment),
    
    URI = uri(
	      Scheme,
	      Userinfo,
	      Host,
	      Port,
	      Path,
	      Query,
	      Fragment).
  


%%%% check functions

check_authority([X, Y | _], CheckAuthority) :-
    X == /,
    Y == /,
    CheckAuthority = 1, !.

check_authority(_, CheckAuthority) :-
    CheckAuthority = 0, !.
    
check_userinfo(List, CheckAuthority, CheckUser) :-
    CheckAuthority == 1,
    member(@, List),
    CheckUser = 1, !.

check_userinfo(_, _,CheckUser) :-
    CheckUser = 0, !.

check_port(List, CheckAuthority, CheckPort) :-
    CheckAuthority == 1,
    member(:, List),
    CheckPort = 1, !.

check_port(_, _, CheckPort) :-
    CheckPort = 0, !.


%%%% parse functions

parse_scheme_mail([: | Tail], Acc, SchemeList, TailList) :-
    Acc \= [],
    out_list(Tail, Acc, SchemeList, TailList), !.

parse_scheme_mail([X | Tail], Acc, SchemeList, TailList) :-
    is_identificatore(X),
    parse_scheme(Tail, [X | Acc], SchemeList, TailList).


parse_userinfo_mail([], [], UserList, TailList) :-
    out_list([], [], UserList, TailList), !.

parse_userinfo_mail([], Acc, UserList, TailList) :-
    out_list([], Acc, UserList, TailList), !.

parse_userinfo_mail([@ | Tail], Acc, UserList, TailList) :-
    Acc \= [],
    out_list([@ | Tail], Acc, UserList, TailList), !.

parse_userinfo_mail([X | Tail], Acc, UserList, TailList) :-
    is_identificatore(X),
    parse_userinfo_mail(Tail, [X | Acc], UserList, TailList).


parse_host_mail([], [], HostList, TailList) :-
    out_list([], [], HostList, TailList), !.

parse_host_mail([@ | Tail], Acc, HostList, TailList) :-
    Tail \= [],
    parse_host_mail(Tail, Acc, HostList, TailList), !.

parse_host_mail([], Acc, HostList, TailList) :-
    Acc \= [],
    out_list([], Acc, HostList, TailList), !.

parse_host_mail([X | Tail], Acc, HostList, TailList) :-
    is_identificatore(X),
    parse_host_mail(Tail, [X | Acc], HostList, TailList).


%%%% news scheme parsing

parse_scheme_news([: | Tail], Acc, SchemeList, TailList) :-
    Acc \= [],
    out_list(Tail, Acc, SchemeList, TailList), !.

parse_scheme_news([X | Tail], Acc, SchemeList, TailList) :-
    is_identificatore(X),
    parse_scheme(Tail, [X | Acc], SchemeList, TailList).


parse_host_news([], [], HostList, TailList) :-
    out_list([], [], HostList, TailList), !.

parse_host_news([], Acc, HostList, TailList) :-
    out_list([], Acc, HostList, TailList), !.

parse_host_news([X | Tail], Acc, HostList, TailList) :-
    parse_host_news(Tail, [X | Acc], HostList, TailList).

%%%% end news parsing


%%%% tel/fax scheme parsing

parse_scheme_tel([: | Tail], Acc, SchemeList, TailList) :-
    Acc \= [],
    out_list(Tail, Acc, SchemeList, TailList), !.

parse_scheme_tel([X | Tail], Acc, SchemeList, TailList) :-
    is_identificatore(X),
    parse_scheme(Tail, [X | Acc], SchemeList, TailList).


parse_userinfo_tel([], [], UserList, TailList) :-
    out_list([], [], UserList, TailList), !.

parse_userinfo_tel([], Acc, UserList, TailList) :-
    out_list([], Acc, UserList, TailList), !.

parse_userinfo_tel([X | Tail], Acc, UserList, TailList) :-
    parse_userinfo_tel(Tail, [X | Acc], UserList, TailList).

%%%% end tel/fax parsing


%%%% zos scheme parsing

parse_scheme_zos([: | Tail], Acc, SchemeList, TailList) :-
    Acc \= [],
    out_list(Tail, Acc, SchemeList, TailList), !.

parse_scheme_zos([X | Tail], Acc, SchemeList, TailList) :-
    is_identificatore(X),
    parse_scheme(Tail, [X | Acc], SchemeList, TailList).


parse_id44([? | Tail], Acc, Id44List, TailList) :-
    out_list([? | Tail], Acc, Id44List, TailList), !.

parse_id44([# | Tail], Acc, Id44List, TailList) :-
    out_list([# | Tail], Acc, Id44List, TailList), !.

parse_id44([], Acc, Id44List, TailList) :-
    out_list([], Acc, Id44List, TailList), !.

parse_id44(['(' | Tail], Acc, Id44List, TailList) :-
    Acc \= [],
    out_list(['(' | Tail], Acc, Id44List, TailList), !.

parse_id44([/ | Tail], Acc, Id44List, TailList) :-
    parse_id44(Tail, Acc, Id44List, TailList).

parse_id44([X | Tail], Acc, Id44List, TailList) :-
    is_id44_safe(X),
    parse_id44(Tail, [X | Acc], Id44List, TailList).


parse_id8([], Acc, Id8List, TailList) :-
    out_list([], Acc, Id8List, TailList), !.

parse_id8([? | Tail], Acc, Id8List, TailList) :-
    out_list(Tail, Acc, Id8List, TailList), !.

parse_id8([# | Tail], Acc, Id8List, TailList) :-
    out_list([# | Tail], Acc, Id8List, TailList), !.

parse_id8([X | Tail], Acc, Id8List, TailList) :-
    is_id8_safe(X),
    parse_id8(Tail, [X | Acc], Id8List, TailList).

%%%% end zos parsing


parse_scheme([: | Tail], Acc, SchemeList, TailList) :-
    Acc \= [],
    out_list(Tail, Acc, SchemeList, TailList), !.

parse_scheme([X | Tail], Acc, SchemeList, TailList) :-
    is_identificatore(X),
    parse_scheme(Tail, [X | Acc], SchemeList, TailList).


parse_userinfo([/, / | Tail], Acc, UserList, TailList, CheckUser) :-
    parse_userinfo(Tail, Acc, UserList, TailList, CheckUser), !.

parse_userinfo(List, [], UserList, TailList, 0) :-
    out_list(List, [], UserList, TailList), !.

parse_userinfo([X | Tail], Acc, UserList, TailList, 1) :-
    is_identificatore(X),
    parse_userinfo(Tail, [X | Acc], UserList, TailList, 1).

parse_userinfo([@ | Tail], Acc, UserList, TailList, _) :-
    Acc \= [],
    out_list(Tail, Acc, UserList, TailList), !.


parse_host([/ | Tail], Acc, HostList, TailList, 1) :-
    Acc \= [],
    out_list([/ | Tail], Acc, HostList, TailList), !.

parse_host([: | Tail], Acc, HostList, TailList, 1) :-
    Acc \= [],
    out_list([: | Tail], Acc, HostList, TailList), !.

parse_host([], Acc, HostList, TailList, 1) :-
    Acc \= [],
    out_list([], Acc, HostList, TailList), !.

parse_host(List, [], HostList, TailList, 0) :-
    out_list(List, [], HostList, TailList), !.

parse_host([X | Tail], Acc, HostList, TailList, 1) :-
    is_identificatore(X),
    parse_host(Tail, [X | Acc], HostList, TailList, 1).


parse_port([: | Tail], Acc, PortList, TailList, 1) :-
    parse_port(Tail, Acc, PortList, TailList, 1), !.

parse_port(List, [], PortList, TailList, 0) :-
    out_list(List, [0, 8], PortList, TailList), !.

parse_port([/ | Tail], Acc, PortList, TailList, 1) :-
    Acc \= [],
    out_list([/ | Tail], Acc, PortList, TailList), !.

parse_port([], Acc, PortList, TailList, 0) :-
    Acc \= [],
    out_list([], Acc, PortList, TailList), !.

parse_port([], [], PortList, TailList, 0) :-
    out_list([], [0, 8], PortList, TailList), !.

parse_port([], Acc, PortList, TailList, 1) :-
    Acc \= [],
    out_list([], Acc, PortList, TailList), !.

parse_port([X | Tail], Acc, PortList, TailList, 1) :-
    is_digit(X),
    parse_port(Tail, [X | Acc], PortList, TailList, 1).


parse_path([? | Tail], Acc, PathList, TailList) :-
    out_list(Tail, Acc, PathList, TailList), !.

parse_path([# | Tail], Acc, PathList, TailList) :-
    out_list([# | Tail], Acc, PathList, TailList), !.

parse_path([], Acc, PathList, TailList) :-
    out_list([], Acc, PathList, TailList), !.

parse_path([X | Tail], Acc, PathList, TailList) :-
    X \= '@',
    X \= ':',
    parse_path(Tail, [X | Acc], PathList, TailList).


parse_query([# | Tail], Acc, QueryList, TailList) :-
    out_list(Tail, Acc, QueryList, TailList), !.

parse_query([], Acc, QueryList, TailList) :-
    out_list([], Acc, QueryList, TailList), !.

parse_query([X | Tail], Acc, QueryList, TailList) :-
    parse_query(Tail, [X | Acc], QueryList, TailList).


parse_fragment([], Acc, FragmentList, TailList) :-
    out_list([], Acc, FragmentList, TailList), !.

parse_fragment([X | Tail], Acc, FragmentList, TailList) :-
    parse_fragment(Tail, [X | Acc], FragmentList, TailList).


%%%% display functions

uri_display(URI) :-
    URI =.. [_, Scheme, Userinfo, Host, Port, Path, Query, Fragment | _],
    write('Scheme: '),
    write(Scheme),
    write('\n'),
    write('Userinfo: '),
    write(Userinfo),
    write('\n'),
    write('Host: '),
    write(Host),
    write('\n'),
    write('Port: '),
    write(Port),
    write('\n'),
    write('Path: '),
    write(Path),
    write('\n'),
    write('Query: '),
    write(Query),
    write('\n'),
    write('Fragment: '),
    write(Fragment), !.

uri_display(URI, Stream) :-
    URI =.. [_, Scheme, Userinfo, Host, Port, Path, Query, Fragment | _],
    write(Stream, 'Scheme: '),
    write(Stream, Scheme),
    write(Stream, '\n'),
    write(Stream, 'Userinfo: '),
    write(Stream, Userinfo),
    write(Stream, '\n'),
    write(Stream, 'Host: '),
    write(Stream, Host),
    write(Stream, '\n'),
    write(Stream, 'Port: '),
    write(Stream, Port),
    write(Stream, '\n'),
    write(Stream, 'Path: '),
    write(Stream, Path),
    write(Stream, '\n'),
    write(Stream, 'Query: '),
    write(Stream, Query),
    write(Stream, '\n'),
    write(Stream, 'Fragment: '),
    write(Stream, Fragment),
    close(Stream).



%%%% utility functions

is_identificatore(X) :-
    X \= '/',
    X \= '?',
    X \= '#',
    X \= '@',
    X \= ':'.

list_reverse(L, R) :-
    aux_reverse(L, R, []).

aux_reverse([], L, L).
aux_reverse([X | Xs], L, Acc) :-
    aux_reverse(Xs, L, [X | Acc]).

out_list(MyList, Acc, Part1, Part2) :-
    list_reverse(Acc, Reversed),
    Part1 = Reversed,
    Part2 = MyList.

member(X, [Y | _]) :-
    X == Y.

member(X, [Y | Tail]) :-
    X \= Y,
    member(X, Tail).

get_ready_for_output(MyList, OutElement) :-
    MyList \= [],
    atomics_to_string(MyList, MyString),
    number_string(OutElement, MyString), !.

get_ready_for_output(MyList, OutElement) :-
    MyList \= [],
    atomics_to_string(MyList, MyString),
    atom_string(OutElement, MyString), !.

get_ready_for_output([], OutElement) :-
    OutElement = [].

recognize_normal_scheme(List) :-
    List \= [m, a, i, l, t, o | Tail],
    List \= [n, e, w, s | Tail],
    List \= [z, o, s | Tail],
    List \= [t, e, l | Tail],
    List \= [f, a, x | Tail], !.

recognize_mailto([m, a, i, l, t, o | _]) :-
    !.

recognize_news([n, e, w, s | _]) :-
    !.

recognize_tel([t, e, l | _]) :-
    !.

recognize_tel([f, a, x | _]) :-
    !.

recognize_zos([z, o, s | _]) :-
    !.
    
is_id44_safe(X) :-
    X == '.'.

is_id44_safe(X) :-
    is_alnum(X).

is_id8_safe(X) :-
    X == '('.

is_id8_safe(X) :-
    X == ')'.

is_id8_safe(X) :-
    is_alnum(X).

ok_id44([]).

ok_id44([X | Center]) :-
    is_alpha(X),
    \+ last(Center, '.'),
    length([X | Center], Len),
    Len =< 44.

ok_id8([]).

ok_id8(['(', X | Center]) :-
    is_alpha(X),
    length([X | Center], Len),
    last(Center, ')'),
    Len =< 9.
    
