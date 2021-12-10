% URI %
uri --> scheme, [:], authority, [/], path, [?], query, [#], fragment.
uri --> scheme, [:], authority, [/], path, [?], query.
uri --> scheme, [:], authority, [/], path, [#], fragment.
uri --> scheme, [:], authority, [/], path.
uri --> scheme, [:], authority, [/].

scheme --> identifier.

% AUTHORITY %
authority --> double_slash, userinfo, [@], host, [:], port.
authority --> double_slash, host, [:], port.
authority --> double_slash, userinfo, [@], host.
authority --> double_slash, host.

host --> host_identifier.
host --> ip_address, !.
host --> host_identifier, [.], host.

userinfo --> identifier.

% PATH %
path --> identifier, [/], path.
path --> identifier.

% QUERY %
query --> [Q], { Q \= '#' }.
query --> [Q], {Q \= '#'}, query.

% FRAGMENT %
fragment --> [F], { char_type(F, ascii) }.
fragment --> [F], { char_type(F, ascii) }, fragment. 

% IDENTIFIERS %
identifier --> [I], { valid_id_char(I) }.
identifier --> [I], { valid_id_char(I) }, identifier.

host_identifier --> [H], { valid_host_char(H) }.
host_identifier --> [H], { valid_host_char(H) }, host_identifier.

% FAI IP %

port --> [P], { char_type(P, digit) }.
port --> [P], { char_type(P, digit) }, port.
port --> [80].

double_slash --> [/], [/].

% VARIE %
uri_parse(URIString, URI) :-
        atom_chars(URIString, List),
        phrase(uri, List).

valid_id_char(Char) :-
        Char \= '/',
        Char \= '?',
        Char \= '#',
        Char \= '@',
        Char \= ':'.

valid_host_char(Char) :-
        valid_id_char(Char),
        Char \= '.'.

valid_ip_number(Char) :-
        char_type(Char, digit),
        Char =< 255,
        Char >= 0.