:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(library(pcre)).

:- http_handler(root(.), list_blogs, []).
:- http_handler(root(blogs), list_blog, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(never).

list_blogs(_Request) :-
    reply_html_page(
        title('BlauAnarchy\'s Blogs'),
        [
            h1('BlauaAnarchy\'s Blogs'),
            table(
                [
                    \header|
                    \blogs(['András Toma', 'Are you Jibaro or Zima Blue', 'Rare kind of listening freedom', 'The recognition of shelter in retrospect'])
                ]
            )
        ]
    ).

header --> 
    html(tr([th('Title'), th('Published At')])).

blogs([]) --> [].
blogs([H|T]) --> 
    html(tr([td(\blog_link(H)), td(\get_published_at(H))])),
    blogs(T).

get_published_at(Blog) -->
    html(now).

blog_link(Blog) -->
    { http_link_to_id(list_blog, [name=Blog], HREF) },
    html(a(href(HREF), Blog)).

list_blog(Request) :-
    http_parameters(Request, [name(Blog, [])]),
    get_blog_file_path(Blog, Path),
    read_blog_files(Path, Paragraphs),
    [Innerparagraphs] = Paragraphs,
    split_string(Innerparagraphs, "\n", "", ParagraphLines),
    render_paragraphs(ParagraphLines, HtmlParagraphs),
    reply_html_page(
        title('Title: ~w'-[Blog]),
        [ 
          div([], HtmlParagraphs)
        ]
    ).

render_paragraphs([], []).
render_paragraphs([Line|Rest], [HTML|Out]) :-
    (
        re_matchsub("^# +(.*)", Line, D, []) -> HTML = h2(D.get(1));
        re_matchsub("^## +(.*)", Line, D, []) -> HTML = h3(D.get(1));
        re_matchsub("^### +(.*)", Line, D, []) -> HTML = h4(D.get(1));
        re_matchsub("^> +(.*)", Line, D, []) -> HTML = blockquote(p(D.get(1)));
        inline(Line, Parts),
        HTML = p(Parts)

    ),
    render_paragraphs(Rest, Out).

    inline(Line, Parts) :-
    (   re_matchsub("^(.*?)\\[(.*?)\\]\\((.*?)\\)(.*)$", Line, D, []) ->
        B = D.get(1),
        T = D.get(2),
        U = D.get(3),
        A = D.get(4),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [a([href(U)], T)|PAfter], Parts)
    ;   re_matchsub("^(.*?)\\*\\*(.*?)\\*\\*(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [b(M)|PAfter], Parts)
    ;   re_matchsub("^(.*?)_(.*?)_(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [i(M)|PAfter], Parts)
    ;   Parts = [Line]
    ).

get_blog_file_path(Blog, Path) :-
    string_lower(Blog, Path0),
    re_replace(" " /g , "_", Path0, Path).

read_blog_files(Blog, Paragraphs) :-
    format(string(Path), "contents/~w.pl", [Blog]),
    consult(Path),
    findall(P, content(P), Paragraphs).
