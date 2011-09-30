:- module(hat, [resolve_template/2,
                join_atoms/2]).

%% resolve_template(+Template, -ResolvedTemplate)
%
% Delegates all heavy lifting to resolve_template_aux/2.
%
resolve_template(Template, ResolvedTemplate) :-
    resolve_template_aux(Template, TemplateTokens),
    join_atoms(TemplateTokens, ResolvedTemplate).

%% resolve_template_aux(+Template, -Tokens)
%
% Calls resolve_template_aux/5 with starter values.
%
resolve_template_aux(TemplateAtom, Tokens) :-
    atom_chars(TemplateAtom, TemplateChars),
    resolve_template_aux(TemplateChars, _, CharsHole-CharsHole, TokensHole-TokensHole, Tokens).

%% resolve_template_aux(+TemplateChars, +TokenType, +CharsDiffList, +TokensDiffList, -Tokens)
%
% Split the template in to a sequence of tokens where
% a token is either a predicate or a span of markup text
% between predicates.
%
% If token is a predicate we construct a unary goal
% and call it.
%

% Case: out of characters, Action: Push last template token onto Tokens
resolve_template_aux([], _, Chars-CharsHole, Tokens-TokensHole, Tokens) :-
    CharsHole = [],
    atom_chars(Token, Chars),
    TokensHole = [Token].

% Case: Opening hat, Action: push chars into tokens and recurse.
resolve_template_aux(['^'|Rest], markup, Chars-[], Tokens-TokensHole, Tokens) :-
    atom_chars(Token, Chars),
    TokensHole = [Token|NewTokensHole],
    resolve_template_aux(Rest, code, NewCharsHole-NewCharsHole, Tokens-NewTokensHole, Tokens).

% Case: Closing hat, Action: construct goal, call it, push return val into tokens, recurse
resolve_template_aux(['^'|Rest], code, Chars-[], Tokens-TokensHole, Tokens) :-
    atom_chars(Token, Chars),
    resolve_template_goal(Token, ResolvedToken),
    TokensHole = [ResolvedToken|NewTokensHole],
    resolve_template_aux(Rest, markup, NewCharsHole-NewCharsHole, Tokens-NewTokensHole, Tokens).

% Case: Other char, Action: Push current char to charstore, recurse.
resolve_template_aux([Char|Rest], State, Chars-[Char|NewCharsHole], Tokens-TokensHole, Tokens) :-
    resolve_template_aux(Rest, State, Chars-NewCharsHole, Tokens-TokensHole, Tokens).


%% resolve_template_goal(+Goal, -ResolvedTemplateGoal)
%
% Creates a callable and calls it, binding output arg.
resolve_template_goal(Goal, ResolvedTemplateGoal) :-
    Callable =.. [Goal, ResolvedTemplateGoal],
    call(Callable).
    
%% join_atoms(+AtomList, -Joined)
%
% Utility predicate for joining lists of atoms.
% SWI has atomic_list_concat in the documentation
% but I don't seem to have it on my system.
%
join_atoms([Last], Last).
join_atoms([Head|Tail], Joined) :-
    join_atoms(Tail, JoinedTail),
    atom_concat(Head, JoinedTail, Joined).

