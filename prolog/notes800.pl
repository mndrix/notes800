:- module(notes800, [caller_id/3]).
:- use_module(library(dcg/basics), [integer//1,string//1,string_without//2]).
:- use_module(library(delay)).
:- use_module(library(rbtrees)).
:- use_module(library(pairs)).
:- use_module(library(solution_sequences)).
:- use_module(library(web), []).
:- use_module(library(xpath)).

% Naming conventions:
%
% `Phone` - a phone number
% `PageN` - the number of an HTML page of results

%% caller_id(+Phone:string,-Caller:string,-Type:atom) is det.
%
%  Determine the Caller and call Type for a given Phone number.
%  If 800notes.com lacks information for Caller, a default value
%  of `"Unknown"` is used; for Type, a default of `unknown`.
caller_id(Phone,Caller,Type) :-
    rb_empty(EmptyCallers),
    rb_empty(EmptyTypes),
    A0 = x(EmptyCallers,EmptyTypes),
    fold_solutions(accum(Name,Value),limit(10,attribute(Phone,Name,Value)),A0,A),
    A= x(Callers,Types),
    once( rb_popular(Callers,Caller); Caller="Unknown" ),
    once( rb_popular(Types,Type); Type=unknown ),
    !.
caller_id(_,"Unknown",unknown).

accum(caller,Caller,x(T0,X),x(T,X)) :-
    !,
    rb_increment(T0,Caller,T).
accum(type,Value,x(X,T0),x(X,T)) :-
    !,
    rb_increment(T0,Value,T).
accum(_,_,Accum,Accum).

rb_increment(T0,Key,T) :-
    ( rb_update(T0,Key,Old,New,T) ->
        succ(Old,New)
    ; otherwise ->
        rb_insert(T0,Key,1,T)
    ).

rb_popular(T,Key) :-
    rb_visit(T,Pairs),
    transpose_pairs(Pairs,LeastToGreatest),
    reverse(LeastToGreatest,[_-Key|_]).


fold_solutions(F,Goal,Accum0,Accum) :-
    Ref = ref(Accum0),
    ( call(Goal),
      Ref = ref(A0),
      call(F,A0,A1),
      nb_setarg(1,Ref,A1),
      fail
    ; Ref = ref(Accum)
    ).


%% attribute(+Phone,-Name,-Value) is multi.
attribute(Phone,Name,Value) :-
    page(Phone,Page),
    comment(Page,Comment),
    detail(Comment,Name,Value).

url(Phone,Url) :-
    url(Phone,1,Url).

url(Phone,PageN,Url) :-
    once(phrase(url(Phone,PageN),Url)).

url(Phone,PageN) -->
    "http://800notes.com",
    path(Phone,PageN).
url(Phone,PageN) -->
    path(Phone,PageN).

%% pager_specific(+Dom,-PageN:integer,-Url:codes)
%
%  Iterates each distinct link pointing to a specific page of results.
pager_specific(Dom,PageN,Url) :-
    distinct([PageN],pager_link_(Dom,PageN,Url)).

pager_link_(Dom,PageN,Url) :-
    xpath(Dom,//div(@id='treeThread')/div(contains(@class,oos_pager))/a(@href=Href),_),
    atom_codes(Href,RelativeUrl),
    url(Phone,PageN,RelativeUrl),
    url(Phone,PageN,Url).

path(Phone,PageN) -->
    { delay(string_codes(Phone,PhoneCodes)) },
    "/Phone.aspx/",
    string_without(`/`,PhoneCodes),
    page_n(PageN).

page_n(1) -->
    "".
page_n(N) -->
    "/",
    integer(N).


%% page(+Phone,-Page)
%
%  Iterates each Page of results for a given Phone from youngest to
%  oldest.
page(Phone,Page) :-
    url(Phone,1,Url),
    web:get(Url,[html5(FirstPage)]),
    once( aggregate(max(N),U^pager_specific(FirstPage,N,U),MaxPageN)
        ; MaxPageN = 1
        ),
    ( page_(Phone,MaxPageN,Page)
    ; Page=FirstPage
    ).

page_(Phone,PageN,Page) :-
    PageN > 1,
    url(Phone,PageN,Url),
    web:get(Url,[html5(Dom)]),
    ( Page=Dom
    ; succ(PrevPageN,PageN),
      page_(Phone,PrevPageN,Page)
    ).


%% comment(+Page,-Comment)
%
%  Iterates each Comment on a Page.
comment(Page,Comment) :-
    xpath(Page,//div(contains(@class,oos_init))/ul/li, Comment).


%% detail(+Comment, -Type, -Value)
%
%  Iterates each Detail within a Comment.
detail(Comment,Type,Value) :-
    xpath(Comment,//div(contains(@class,callDetails))/div(text),Text),
    atom_codes(Text,Codes),
    phrase(detail(Type,Value),Codes).

detail(caller,Value) -->
    "Caller: ",
    string(Codes),
    { string_codes(Value,Codes) }.
detail(type,Value) -->
    "Call Type: ",
    string(Raw),
    { maplist(transform_type,Raw,Transformed) },
    { atom_codes(Value,Transformed) }.

transform_type(0' ,0'_) :- !.
transform_type(U,L) :-
    code_type(U,upper(L)),
    !.
transform_type(C,C).
