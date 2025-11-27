# Plog — A Prolog Blog Engine
## Dynamic Markdown -> HTML server in pure Prolog.

Yes, you can write a web server with Prolog. 

Plog is a small, self-contained experiment: a blog engine written in pure Prolog that dynamically reads Markdown files, parses them at request time, and serves clean HTML using a minimal prolog HTTP server.

No frameworks. No dependencies. No JavaScript. Just Prolog, a few predicates, and a folder of .md files.

### Features

- Dynamic: reads and parses Markdown on every request

- Pure Prolog: no external libraries (except the standard HTTP package)

- Simple HTTP server: ~70 lines of logic for routing + rendering

- Markdown support: headings, paragraphs, and links

- Automatic HTML generation

Good as a learning resource for Prolog + web development

### Why Prolog?

Prolog is rarely used for web servers, which makes it perfect for small, self-contained explorations like this.

What surprised me while building it is how naturally Prolog handles structural parsing, declarative transformations, and expressing a page as logic. It uses clean pattern-matching instead of string juggling.

Plog is “What does a blog engine look like if you write it in logic?”

### Getting Started
Run the server
```bash
swipl -s main.pl -g server(5001)
```
Then open:
`http://localhost:5001`, which contains links to each blog's page.

Add your own posts
Place Markdown files into:
`contents/`, wrap them like this:
```prolog
content("your content in markdown").
```
Each file can use:
```
# Heading
## Subheading
### Sub-subheading
plain paragraphs
```
That's it!

### Example Markdown
```
# Hello World
## This is my first post in Prolog
### history of prolog
...
### the power of prolog
...
```
Becomes:

```
<h2>Hello World</h2>
<h3>This is my first post in Prolog.<h3>
<h4>history of prolog<h3>
<p>...</p>
<p>the power of prolog</p>
<p>...</p>
```

### Code Philosophy

The project intentionally avoids complexity. No abstractions unless justified. 
Everything is visible and understandable at a glance.

If you enjoy:
- reading compact, understandable codebases
- learning by observing a working end-to-end example
- building unusual things in unusual languages
- seeing logic programming do practical work

then you might find this fun to explore.

### Future Works

- Static site generation (export/0)
- Support more Markdown features
- tagging + RSS

⭐ If you find it interesting
If this little exploration inspires you, feel free to ⭐ the repo.
It helps others discover it, and I may continue extending it if people enjoy reading it.
