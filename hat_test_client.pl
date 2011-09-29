:- use_module('hat.pl').
:- style_check(-atom).

%%
% 'Main' testing predicate.
test_templating :-
    test_template(Template),
    resolve_template(Template, ResolvedTemplate),
    format('ResolvedTemplate: \n~w\n', [ResolvedTemplate]).

%%
% We store the template here.
test_template('
<html>
  <head>
    <title>^page_title^</title>
  </head>
  <body>
    <table id="books-table" class="hat-template">
        ^make_table_rows^
    </table>
  </body>
</html>
').

%%
% Page title
page_title('Great Prolog Books').

%%
% Template callback
make_table_rows(ResolvedTemplate) :-
    setof([Author, Book], author_of(Author, Book), Pairs),
    maplist(make_table_row, Pairs, TableRows),
    join_atoms(TableRows, ResolvedTemplate).

%%
% Callback helper predicate
make_table_row([Author, Book], Row) :-
    sformat(Row, '      <tr><td>~w</td><td>~w</td></tr>\n', [Author, Book]).

%%
% Data for the table rows.
%
author_of('Pereira/Shieber', 'Prolog and NL Analysis').
author_of('Sterling/Shapiro', 'Art of Prolog').
author_of('Clocksin/Mellish', 'Programming in Prolog').
author_of('Okeefe', 'Craft of Prolog').
author_of('Bratko', 'Prolog Programming for AI').

:- test_templating.
