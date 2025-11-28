# Plog - A Prolog Blog Engine

<div align="left">

[![Prolog](https://img.shields.io/badge/SWI--Prolog-9.x-6B2FBF?logo=prolog&logoColor=white)](https://www.swi-prolog.org/)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/cryptoque/socratic_mirror/pulls)

</div>

What does a blog engine look like if you write it in logic?

### Dynamic Markdown -> HTML server in pure Prolog.

Yes, you can write a web server with Prolog. 

Plog is a blog engine written in pure Prolog that dynamically reads Markdown files, parses them at request time, and serves clean HTML using a minimal prolog HTTP server.

No frameworks. No dependencies. No JavaScript. Just Prolog. 

To add a new blog entry, simply write it in markdown and add it as prolog file to the contents folder. The prolog engine will dynamically parse the markdown into html recursively for display.

Check the site live: https://blauanarchy.org

<table align="center">
  <tr>
    <td><img src="https://github.com/user-attachments/assets/d0e483cd-b9d0-4e05-89c2-9035f9b3aaf7" width="400"></td>
    <td><img src="https://github.com/user-attachments/assets/928f50dd-9a44-42c8-ae5e-a5ec24566df5" width="400"></td>
  </tr>
  <tr>
    <td><img src="https://github.com/user-attachments/assets/7e3aa66e-085d-450d-ac43-2d5ec6555791" width="400"></td>
    <td><img src="https://github.com/user-attachments/assets/ae0b4403-48ef-484e-8913-46063717c10a" width="400"></td>
  </tr>
</table>

### Features

- Dynamically reads and parses Markdown on every request.

- Pure Prolog: no external libraries or framework (except the standard HTTP package).

- Prolog HTTP server: routing + rendering.

- Prolog markdown parser supports: All headings, paragraphs, links, `code block`, **bold**, _italic_, blockquote, images, horizontal rule. 

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

This project intentionally avoids complexity. No abstractions unless justified. 
Everything is visible and understandable at a glance.

### Future Works

- Static site generation (export/0)
- Support more Markdown features
- RSS

⭐ If you find it interesting
If this little exploration inspires you, feel free to ⭐ the repo.
