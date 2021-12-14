% URI %
uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
        scheme(Scheme),
        after_scheme(Userinfo, Host, Port, Path, Query, Fragment).

scheme(Scheme) --> identifier(Scheme), [:].

after_scheme(Userinfo, Host, Port, Path, Query, Fragment) -->
        authority(Userinfo, Host, Port), 
        after_authority(Path, Query, Fragment).
after_scheme([], [], [], [], [], []) --> [].

% AUTHORITY %
authority(Userinfo, Host, Port) --> double_slash, userinfo(Userinfo),
        host(Host), port(Port).

after_authority(Path, Query, Fragment) --> [/], path(Path), 
        query(Query), fragment(Fragment).
after_authority([], [], []) --> [].

% HOST %
host(Host) --> host_identifier(HA), [.], host(HB), !, 
        { append([HA, ['.'], HB], Host) }.
host(Host) --> host_identifier(Host), !.
host(Host) --> ip(Host).

% USER INFO %
userinfo(Userinfo) --> identifier(Userinfo), [@].
userinfo([]) --> [].

% PATH %
path(Path) --> identifier(PA), [/], path(PB), !,
        { append([PA, ['/'], PB], Path)}.
path(Path) --> identifier(Path).
path([]) --> [].

% QUERY %
query(Query) --> [?], valid_query(Query).
query([]) --> [].

valid_query(Query) --> [Q_head], { Q_head \= '#' }, 
        valid_query(Q_rest), { Query = [Q_head | Q_rest] }.
valid_query(Query) --> [Q_head], { Q_head \= '#', Query = [Q_head] }.

% FRAGMENT %
fragment(Fragment) --> [#], valid_fragment(Fragment).
fragment([]) --> [].

valid_fragment(Fragment) -->  [F_head], 
        valid_fragment(F_rest), { Fragment = [F_head | F_rest] }.
valid_fragment(Fragment) --> [F_head], { Fragment = [F_head] }.

% IDENTIFIERS %
identifier(I) --> [I_head], { valid_id_char(I_head) }, 
                identifier(I_rest), !, { I = [I_head | I_rest] }.
identifier(I) --> [I_head], { valid_id_char(I_head), I = [I_head] }.

% HOST IDENTIFIER %
host_identifier(H) --> [H_head], { valid_host_char(H_head) }, 
        host_identifier(H_rest), !, { H = [H_head | H_rest] }.
host_identifier(H) --> [H_head], { valid_host_char(H_head), H = [H_head] }.

% IP %
ip(Ip) --> ip_number(D1),
        [.], ip_number(D2),
        [.], ip_number(D3),
        [.], ip_number(D4),
        { append([D1, [.], D2, [.], D3, [.], D4], Ip) }.
ip([]) --> [].

ip_number(Dig) --> [D1, D2, D3], 
        { valid_ip_number(D1, D2, D3), Dig = [D1, D2, D3] }.

% PORT %
port(Port) --> [:], !, port_valid(Port).
port(['8', '0']) --> [], !.

port_valid(Port) --> [P_head], { char_type(P_head, digit) },
        port_valid(P_rest), !, { Port = [P_head | P_rest] }.
port_valid(Port) --> [P_head], { char_type(P_head, digit), 
        Port = [P_head] }, !.

% VARIE %
double_slash --> [/], [/].

% VARIE %
uri_parse(URIString, uri(Sch, UI, Ho, Po, Pa, Qu, Fr)) :-
        string_chars(URIString, List),
        phrase(uri(Sch, UI, Ho, Po, Pa, Qu, Fr), List).

valid_id_char(Char) :-
        Char \= '/',
        Char \= '?',
        Char \= '#',
        Char \= '@',
        Char \= ':'.

valid_host_char(Char) :-
        valid_id_char(Char),
        Char \= '.'.

valid_ip_number(D1, D2, D3) :-
        char_type(D1, digit),
        char_type(D2, digit),
        char_type(D3, digit),
        number_string(Number, [D1, D2, D3]),
        Number >= 0,
        Number =< 255.

% TEST %
test_uri_1_success :-
        uri_parse("http://www.google.com/we/dir?query#fragment", _).
test_uri_2_success :-
        uri_parse("http://www.google.com/we/dir?query", _).
test_uri_3_success :-
        uri_parse("http://www.google.com/we/dir", _).
test_uri_4_success :-
        uri_parse("http://www.google.com/", _).
test_uri_5_success :-
        uri_parse("http://www.google.com/we/", _).
test_uri_6_success :-
        uri_parse("http:", _).