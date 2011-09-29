:- module(hat, [resolve_template/2,
                join_atoms/2]).

resolve_template(Template, ResolvedTemplate) :-
    resolve_template_aux(Template, TemplateTokens),
    join_atoms(TemplateTokens, ResolvedTemplate).




%%
% Split the template in to a sequence of tokens where
% a token is either a callable or a span of markup text
% between callables.
resolve_template_aux(TemplateAtom, Tokens) :-
    atom_chars(TemplateAtom, TemplateChars),
    resolve_template_aux(TemplateChars, _, CharsHole-CharsHole, TokensHole-TokensHole, Tokens).
% Case: out of characters , Action: Push last template token onto Tokens
resolve_template_aux([], _, Chars-CharsHole, Tokens-TokensHole, Tokens) :-
    format('Hit empty list.\n'),
    CharsHole = [],
    atom_chars(Token, Chars),
    TokensHole = [Token].
% Case: Opening hat , Action: join chars into tok, put tok in tokens, clear chars.
resolve_template_aux(['^'|Rest], markup, Chars-[], Tokens-TokensHole, Tokens) :-
    format('Beginning of callable\n'),
    atom_chars(Token, Chars),
    TokensHole = [Token|NewTokensHole],
    resolve_template_aux(Rest, code, NewCharsHole-NewCharsHole, Tokens-NewTokensHole, Tokens).
% Case: Closing hat , Action: join chars into tok, put tok in tokens, clear chars.
resolve_template_aux(['^'|Rest], code, Chars-[], Tokens-TokensHole, Tokens) :-
    format('End of callable\n'),
    atom_chars(Token, Chars),
    resolve_template_goal(Token, ResolvedToken),
    TokensHole = [ResolvedToken|NewTokensHole],
    resolve_template_aux(Rest, markup, NewCharsHole-NewCharsHole, Tokens-NewTokensHole, Tokens).
% CurrentChar == nonspecial char , Action: Push current char to charstore.
resolve_template_aux([Char|Rest], State, Chars-[Char|NewCharsHole], Tokens-TokensHole, Tokens) :-
    format('Other clause:[~w]\n', [Char]),
    resolve_template_aux(Rest, State, Chars-NewCharsHole, Tokens-TokensHole, Tokens).

%%
% Creates a callable and calls it.
resolve_template_goal(Goal, ResolvedTemplateGoal) :-
    Callable =.. [Goal, ResolvedTemplateGoal],
    call(Callable).
    
%%
%
join_atoms([Last], Last).
join_atoms([Head|Tail], Joined) :-
    join_atoms(Tail, JoinedTail),
    atom_concat(Head, JoinedTail, Joined).

