# Plog — A Prolog Blog Engine

What does a blog engine look like if you write it in logic?

### Dynamic Markdown -> HTML server in pure Prolog.

Yes, you can write a web server with Prolog. 

Plog is a self-contained experiment: a blog engine written in pure Prolog that dynamically reads Markdown files, parses them at request time, and serves clean HTML using a minimal prolog HTTP server.

No frameworks. No dependencies. No JavaScript. Just Prolog, and a folder of markdown files. 

To add a new blog entry, simply write it in markdown and add it as prolog file to the content folder. The prolog engine will dynamically parse the markdown into html recursively for display.

Check the site live: https://blauanarchy.org

### Features

- Dynamically reads and parses Markdown on every request.

- Pure Prolog: no external libraries or framework (except the standard HTTP package).

- Prolog HTTP server: routing + rendering.

- Prolog markdown parser supports: All headings, paragraphs, links, `code block`, **bold**, _italic_, blockquote, images, horizontal rule. 

- Automatic HTML generation.

### Getting Started
Run the server
```bash
swipl -s main.pl -g server(YOUR_PORT)
```
Then open:
`http://localhost:YOUR_PORT`, which contains the index page for the blogs and links to each individual blog page.

Add your own posts: wrap your own markdown file like this:
```prolog
content("your content in markdown").
```
Each file can use:
```
# Heading
## Subheading
### Sub-subheading
plain paragraphs
bold
italic
links
images
horizontal rule
code block
```
That's it!

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
