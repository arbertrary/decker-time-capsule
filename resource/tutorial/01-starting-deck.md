---
title: Guide to Presenting with Decker
---

# General Workflow

Decker is a command line tool. Begin with an existing project or use `decker example` to generate an example project.

1.  Navigate to your project directory.
2.  Use `decker server` to create html pages and open a local server.
3.  In a browser window navigate to: `localhost:8888`.
4.  Decker uses markdown files ending in `-deck.md`. Create or edit these files and see changes in the browser window on file save.
5.  When finished, shut down the server by pressing `Control C` on the command line.

# Folder Structure

- When running, decker creates a `public` folder in the current directory.
- This folder contains the template and support files needed during presentation time as well as the generated `html` and `pdf` files.

# Menu

When viewing a Decker presentation, click the menu <i class="fas fa-bars"></i> to see a table of contents. To disable the menu, enter `menu: false` in [decker.yaml](#yaml).

# Search

Open the menu <i class="fas fa-bars"></i> and click <i class="fas fa-search"></i> to open a search field. Enter the search term and push `enter` to view slides containing the search term. Push `esc` to close the search field.

# Print PDF

Open the menu <i class="fas fa-bars"></i> and click <i class="fas fa-print"></i> to save the presentation as a PDF document.

# Whiteboard

Use the Whiteboard to dynamically make notes on presentations.

## {.small}

| Icon / Key                                                           | Function               |
| :------------------------------------------------------------------- | :--------------------- |
| <i class="fas fa-pen"></i>                                           | make notes on slides   |
| <i class="fas fa-eraser"></i>                                        | use an eraser          |
| <i class="fas fa-edit"></i>                                          | open the whiteboard    |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i>             | draw on the whiteboard |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i> + `<ENTER>` | extend the whiteboard  |
| <i class="fas fa-magic"></i>                                         | use a laser pointer    |
| <i class="fas fa-save"></i>                                          | save whiteboard notes  |
| `<del>`                                                              | clear slide            |

# Save Notes & Drawings

Click <i class="fas fa-save"></i> to save slide notes and whiteboard drawings. A new whiteboard file \* will be created in your project directory ending in -annot.json:

## {.small}

```{.yaml}
> project directory
    > audio
    > imgs
    decker.yaml
    example-deck.md
    example-annot.json *
```

# Using Whiteboard Files

To include these notes and drawings in your next presentation, note the file in [YAML meta](#yaml):

## {.small}

```{.yaml}
title: Decker Example
whiteboard: 'example-annot.json'
```

# Speaker Notes

Use the `{.notes}` tag to create notes that appear in the speaker view. The slide is used as the speaker notes for the slide above it. (Press **`s`** to access speaker view.)

## {.small}

```{.yaml}
# Why Gamify? {.notes}

- Games are among the most powerful motivational tools.
- Make the non-game experience more rewarding.
- Motivation has limits. A large leaderboard divide may
    cause the player to abandon the game.
```

# Why Gamify? {.notes}

- Games are among the most powerful motivational tools.

- Make the non-game experience more rewarding

- Motivation has limits. A large leaderboard divide may cause the player to abandon the game.

# Other Features {.columns}

## {.top}

Use these keys to try out the other Decker features:

## {.definition .left}

`o` - Slide Overview\
`l` - Laser Pointer\
`b` - Blackout\
`f` - Fullscreen

## {.right}

## {.bottom}

Double-click on any element in your presentation to zoom in.\
Double-click again to zoom-out.

# Keyboard Shortcut Menu

## {.small .column width="24%"}

| Key  | Direction |
| :--- | :-------- |
| `←`  | left      |
| `→`  | right     |
| `↑`  | up        |
| `↓`  | down      |
| n    | next      |
| p    | previous  |
| home | first     |
| end  | last      |

## {.small .column width="41%"}

| Key             | Command         |
| :-------------- | :-------------- |
| f               | fullscreen      |
| s               | speaker notes   |
| m               | toggle menu     |
| ctrl + shft + f | toggle search   |
| q               | toggle quiz     |
| o               | toggle overview |
| b               | toggle blackout |
| esc             | escape feature  |

## {.small .column width="33%"}

| Key    | Command           |
| :----- | :---------------- |
| w      | toggle whiteboard |
| d      | toggle drawing    |
| e      | toggle eraser     |
| l      | toggle laser      |
| delete | clear slide       |

# Configuration Options {#yaml}

Options to configure your presentation are defined in YAML metadata in the `decker.yaml` file. Override these options in your markdown file.

i.e. located at the top of the `*.md` file:

## {.small}

```{.yaml}
---
title: Decker Slide Tool Reference Guide
vertical-slides: true
menu: false
bibliography: example.bib
csl: chicago-author-date.csl
---
```

# Configuration Options

## {.small}

| Parameter         | Options | Effect                                                                 |
| ----------------- | ------- | ---------------------------------------------------------------------- |
| `author`          | String  | Displayed on first slide                                               |
| `date`            | String  | Displayed on first slide (if "today" shows current date as YYYY-MM-DD) |
| `title`           | String  | Displayed on first slide                                               |
| `subtitle`        | String  | Displayed on first slide                                               |
| `width`, `height` | numeric | Define aspect ratio                                                    |

# Configuration Options

## {.small}

| Parameter     | Options           | Effect                                 |
| ------------- | ----------------- | -------------------------------------- |
| `menu`        | `true` or `false` | Include menu showing table of contents |
| `progress`    | `true` or `false` | Turn progress bar on/off               |
| `slideNumber` | `true` or `false` | Turn slide numbers on/off              |
| `history`     | `true` or `false` | Show slides in browser history         |
| `controls`    | `true` or `false` | Turn arrow controls on/off             |
| `whiteboard`  | `true` or `false` | Include reveal.js whiteboard plugin    |
| `chart`       | `true` or `false` | Include reveal.js chart plugin         |

# Configuration Options

## {.small}

| Parameter      | Options                          | Effect                          |
| -------------- | -------------------------------- | ------------------------------- |
| `csl`          | Filepath to .csl file            | Include a citation style (.csl) |
| `bibliography` | Filepath to .bib file            | Include bibliography            |
| `css`          | Filepath to .css file            | Additional CSS resources        |
| `lang`         | Any ISO Language Code (eg. `de`) | HTML content language           |
| `dir`          | `RTL` or `LTR`                   | Text content direction          |

# Decker Commands

Use on the command line after the word `decker` (ie `decker clean`)

## {.definition .small}

| Command   | Function                                                                                                                                                    |
| :-------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `clean`   | removes all generated files from the `public` directory and clears cached resource folders with version number older than the currently used decker version |
| `decks`   | creates only HTML slide decks                                                                                                                               |
| `example` | copies an example project to the current directory                                                                                                          |
| `help`    | prints a help document to stdout in Markdown format                                                                                                         |
| `html`    | creates all HTML files without opening a server                                                                                                             |

# Decker Commands

## {.definition .small}

| Command     | Function                                                                                                                                                 |
| :---------- | :------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `info`      | prints information about the current project's directories, files that will be generated and meta data options found in the top level `decker.yaml` file |
| `pdf`       | creates pdf version of all files                                                                                                                         |
| `pdf-decks` | creates pdf versions only of the html slide decks                                                                                                        |
| `publish`   | publish generated files to a remote location using `rsync` \*\*                                                                                          |

## {.small}

\*\*_The keys `rsync-destination.host` and `rsync-destination.path` in YAML metadata must specify the publishing destination._

# Decker Commands

## {.definition .small}

| Command  | Function                                                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `server` | like `decker watch` but this additionally starts a local web server at the address `localhost:8888` and serves generated HTML files. Changed files are reloaded in the browser. |
| `watch`  | builds HTML versions of all documents and watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `Ctrl C`          |
