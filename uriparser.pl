:- set_prolog_flag(double_quotes, chars).

% URI %
uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) -->
        scheme(Scheme), 
        after_scheme(Scheme, Userinfo, Host, Port, Path, Query, Fragment).

uri(Scheme, [], [], Port, Path, Query, Fragment) -->
        scheme(Scheme), [/], path(Path),
        query(Query), fragment(Fragment), port(Port).

% SCHEME %
scheme(Scheme) --> identifier(Scheme_ch),
        { atom_chars(Scheme, Scheme_ch) }, [:].

% AFTER SCHEME %
after_scheme(mailto, Userinfo, [], Port, [], [], []) -->
        userinfo(Userinfo), port(Port).
after_scheme(mailto, Userinfo, Host, Port, [], [], []) -->
        userinfo(Userinfo), [@], host(Host), port(Port).

after_scheme(news, [], Host, Port, [], [], []) -->
        host(Host), port(Port).

after_scheme(tel, Userinfo, [], Port, [], [], []) -->
        userinfo(Userinfo), port(Port).
after_scheme(fax, Userinfo, [], Port, [], [], []) -->
        userinfo(Userinfo), port(Port).

after_scheme(zos, Userinfo, Host, Port, Path, Query, Fragment) --> 
        authority(Userinfo, Host, Port),
        after_authority_zos(Path, Query, Fragment), !.

after_scheme(_, Userinfo, Host, Port, Path, Query, Fragment) -->
        authority(Userinfo, Host, Port), 
        after_authority(Path, Query, Fragment).

% AUTHORITY %
authority(Userinfo, Host, Port) --> double_slash, userinfo(Userinfo), [@],
        host(Host), port(Port).
authority([], Host, Port) --> double_slash, host(Host), port(Port).
authority([], [], Port) --> [], port(Port).

after_authority(Path, Query, Fragment) --> [/], path(Path), 
        query(Query), fragment(Fragment).
after_authority([], [], []) --> [].

after_authority_zos(Path, Query, Fragment) --> [/], path_zos(Path), 
        query(Query), fragment(Fragment).
after_authority_zos([], [], []) --> [].

% HOST %
host(Host) --> host_identifier(HA_ch), [.], host(HB),
        { atom_chars(HA, HA_ch), atomic_list_concat([HA, '.', HB], Host) }.
host(Host) --> host_identifier(Host_ch), { atom_chars(Host, Host_ch)}.
host(Host) --> ip(Host).

% USER INFO %
userinfo(Userinfo) --> identifier(Userinfo_ch), 
        { atom_chars(Userinfo, Userinfo_ch)}.

% PATH %
path(Path) --> valid_path(Path_ch), { atom_chars(Path, Path_ch) }.
path([]) --> [].

valid_path(Path) --> identifier(PA), [/], valid_path(PB), 
        { append([PA, ['/'], PB], Path) }.
valid_path(Path) --> identifier(Path).

path_zos(Path) --> valid_path_zos(Path_ch), { atom_chars(Path, Path_ch) }.

valid_path_zos(Path) --> id44(PZA), { length(PZA, L), L =< 44 },
        ['('], id8(PZB), { length(PZB, L2), L2 =< 8}, [')'],
        { append([PZA, ['('], PZB, [')']], Path) }.
valid_path_zos(Path) --> id44(Path).

% ID44 %
id44(Id) --> valid_id44(IdA), [.], id44(IdB), {append([IdA, ['.'], IdB], Id)}.
id44(Id) --> [Id1], { char_type(Id1, alpha) },
        valid_id44(Id_rest), 
        { Id = [Id1 | Id_rest] }.
id44(Id) --> [Id1], { char_type(Id1, alpha), Id = [Id1] }.

valid_id44(Id) --> [Id_head],
        {char_type(Id_head, alnum) },
        valid_id44(Id_rest), 
        { Id = [Id_head | Id_rest] }.
valid_id44(Id) --> [Id_head], { char_type(Id_head, alnum), Id = [Id_head] }.

% ID8 %
id8(Id) --> [Id1], { char_type(Id1, alnum) }, 
        valid_id8(Id_rest), 
        { Id = [Id1 | Id_rest] }.
id8(Id) --> [Id1], { char_type(Id1, alpha), Id = [Id1] }.

valid_id8(Id) --> [Id_head], valid_id44(Id_rest), 
        { Id = [Id_head | Id_rest] }.
valid_id8(Id) --> [Id_head], { Id = [Id_head] }.

% QUERY %
query(Query) --> [?], valid_query(Query_ch), { atom_chars(Query, Query_ch)}.
query([]) --> [].

valid_query(Query) --> [Q_head], { Q_head \= '#' }, 
        valid_query(Q_rest), { Query = [Q_head | Q_rest] }.
valid_query(Query) --> [Q_head], { Q_head \= '#', Query = [Q_head] }.

% FRAGMENT %
fragment(Fragment) --> [#], valid_fragment(Fragment_ch), 
        { atom_chars(Fragment, Fragment_ch)}.
fragment([]) --> [].

valid_fragment(Fragment) -->  [F_head], 
        valid_fragment(F_rest), { Fragment = [F_head | F_rest] }.
valid_fragment(Fragment) --> [F_head], { Fragment = [F_head] }.

% IDENTIFIERS %
identifier(I) --> [I_head], { valid_id_char(I_head) }, 
                identifier(I_rest), { I = [I_head | I_rest] }.
identifier(I) --> [I_head], { valid_id_char(I_head), I = [I_head] }.

% HOST IDENTIFIER %
host_identifier(H) --> [H_head], { valid_host_char(H_head) }, 
        host_identifier(H_rest), { H = [H_head | H_rest] }.
host_identifier(H) --> [H_head], { valid_host_char(H_head), H = [H_head] }.

% IP %
ip(Ip) --> ip_number(D1_ch),
        [.], ip_number(D2_ch),
        [.], ip_number(D3_ch),
        [.], ip_number(D4_ch),
        {
                atom_chars(D1, D1_ch),
                atom_chars(D2, D2_ch),
                atom_chars(D3, D3_ch),
                atom_chars(D4, D4_ch),
                atomic_list_concat([D1, '.', D2, '.', D3, '.', D4], Ip) 
        }.

ip_number(Dig) --> [D1, D2, D3], 
        { valid_ip_number(D1, D2, D3), Dig = [D1, D2, D3] }.

% PORT %
port(Port) --> [:], port_valid(Port_ch), { atom_chars(Port, Port_ch)}.
port('80') --> [].

port_valid(Port) --> [P_head], { char_type(P_head, digit) },
        port_valid(P_rest), { Port = [P_head | P_rest] }.
port_valid(Port) --> [P_head], { char_type(P_head, digit), 
        Port = [P_head] }.

% VARIE %
double_slash --> [/], [/].

% VARIE %
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

handle_whitespace([], []).
handle_whitespace([' ' | St], [['%', '2', '0'] | NSt]) :- handle_whitespace(St, NSt).
handle_whitespace([S | St], [S | NSt]) :-
        dif(S, ' '),
        handle_whitespace(St, NSt).

uri_parse(URIString, uri(Sch, UI, Ho, Po, Pa, Qu, Fr)) :-
        handle_whitespace(URIString, WhiteURI),
        flatten(WhiteURI, NormURI),
        phrase(uri(Sch, UI, Ho, Po, Pa, Qu, Fr), NormURI).

% TEST %
test_success :-
        uri_parse("http://www.google.com/we/dir?query#fragment", _),
        uri_parse("http://www.google.com/we/dir?query", _),
        uri_parse("http://www.google.com/we/dir", _),
        uri_parse("http://www.google.com/we", _),
        uri_parse("http://www.google.com/", _),
        uri_parse("http:", _).