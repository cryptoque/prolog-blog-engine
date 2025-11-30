% Copyright © 2025 Zhongying Qiao
% Licensed under the Apache License 2.0.
% See the LICENSE file for details or http://www.apache.org/licenses/LICENSE-2.0.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(library(pcre)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_files)).
:- use_module(style).

:- http_handler(root(.), list_blogs, []).
:- http_handler(root(blogs), list_blog, []).

:- multifile http:location/3.
http:location(images, root(images), []).
:- multifile http:location/3.
http:location(contents, root(contents), []).

:- multifile user:file_search_path/2.
user:file_search_path(images, 'images').
:- multifile user:file_search_path/2.
user:file_search_path(contents, 'contents').

:- http_handler(images(.), image_handler, [prefix]).
:- http_handler(contents(.), content_handler, [prefix]).

image_handler(Request) :-
    http_reply_from_files('images', [], Request).
content_handler(Request) :-
    http_reply_from_files('contents', [], Request).

content_files(Files) :-
    absolute_file_name(contents, Dir, [ file_type(directory), access(read)]),
    directory_files(Dir, Raw),
    exclude(is_dot, Raw, Files).
is_dot('.').
is_dot('..').

file_info(File, Size, Modified) :-
    format(string(Path), "contents/~w", [File]),
    size_file(Path, Size),
    time_file(Path, Modified).

format_timestamp(Stamp, Time) :-
    stamp_date_time(Stamp, DT, 'UTC'),
    format_time(string(Time), "%Y-%m-%d %H:%M:%S", DT).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(never).

list_blogs(_Request) :-
    content_files(Files),
    predsort(compare, Files, Sorted),
    reply_html_page(
        title('BlauAnarchy\'s Blogs'),
        [ \page_style ],
        [
            div([id(content)], [
                h1('BlauAnarchy\'s Blogs'),
                div([id(meta)], [
                    span('This page was assembled dynamically by a SWI-Prolog server using the http/html_write library.
                    The system reads Markdown files from the contents/ directory, converts them into structured HTML through a Markdown parser written from scratch, and generates the layout at runtime without templates or external frameworks. '),
                    a([href('https://github.com/cryptoque/prolog-blog-engine'), target('_blank')], '\nCheck out the source on GitHub')
                ]),

                table(
                    [
                        \header|
                        \blogs(Sorted)
                    ]
                )
            ])
        ]
    ).

header --> 
    html(tr([th('Title'), th('Last Updated At')])).

blogs([]) --> [].
blogs([H|T]) --> 
    {get_blog_display_name(H, H0)},
    html(tr([td(\blog_link(H0, H)), td(\get_published_at(H))])),
    blogs(T).

get_published_at(Blog) -->
    { file_info(Blog, _, Created) },
    { format_timestamp(Created, CreatedFormatted) },
    html(CreatedFormatted).

blog_link(Blog, Display) -->
    { http_link_to_id(list_blog, [name=Display], HREF) },
    html(a(href(HREF), Blog)).

list_blog(Request) :-
    http_parameters(Request, [name(Blog, [])]),
    read_blog_files(Blog, Paragraphs),
    [Innerparagraphs] = Paragraphs,
    split_string(Innerparagraphs, "\n", "", ParagraphLines),
    render_paragraphs(ParagraphLines, HtmlParagraphs),
    reply_html_page(
        title('Title: ~w'-[Blog]),
        [ \blog_style ],
        [ 
          div(id(content), HtmlParagraphs)
        ]
    ).

render_paragraphs([], []).
render_paragraphs([Line|Rest], [HTML|Out]) :-
    (
        re_matchsub("^# +(.*)", Line, D, []) -> HTML = h1(D.get(1));
        re_matchsub("^## +(.*)", Line, D, []) -> HTML = h2(D.get(1));
        re_matchsub("^### +(.*)", Line, D, []) -> HTML = h3(D.get(1));
        re_matchsub("^> +(.*)", Line, D, []) -> HTML = blockquote(p(D.get(1)));
        re_matchsub("^---$", Line, _, []) -> HTML = hr([]);
        inline(Line, Parts),
        HTML = p(Parts)

    ),
    render_paragraphs(Rest, Out).

    inline(Line, Parts) :-
    (   re_matchsub("^(.*?)\\!\\[(.*?)\\]\\((.*?)\\)(.*)$", Line, D, []) ->
        B = D.get(1),
        T = D.get(2),
        U = D.get(3),
        A = D.get(4),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [img([src(U), width('450'), height('400'), alt(T)])|PAfter], Parts);
   
        re_matchsub("^(.*?)\\[(.*?)\\]\\((.*?)\\)(.*)$", Line, D, []) ->
        B = D.get(1),
        T = D.get(2),
        U = D.get(3),
        A = D.get(4),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [a([href(U)], T)|PAfter], Parts);
   
        re_matchsub("^(.*?)`(.*?)`(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [code(M)|PAfter], Parts);

        re_matchsub("^(.*?)\\*\\*(.*?)\\*\\*(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [b(M)|PAfter], Parts);
    
        re_matchsub("^(.*?)_(.*?)_(.*)$", Line, D, []) ->
        B = D.get(1),
        M = D.get(2),
        A = D.get(3),
        inline(B, PBefore),
        inline(A, PAfter),
        append(PBefore, [i(M)|PAfter], Parts);
        
        Parts = [Line]
    ).

get_blog_display_name(Blog, Path) :-
    re_replace("_" /g , " ", Blog, Path0),
    re_replace(".pl" /g , "", Path0, Path1),
    string_upper(Path1, Path).

read_blog_files(Blog, Paragraphs) :-
    format(string(Path), "contents/~w", [Blog]),
    consult(Path),
    findall(P, content(P), Paragraphs).
