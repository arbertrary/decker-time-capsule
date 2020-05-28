---
title: Guide to Presenting with Decker
margin-columns: -0.15
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

Click <i style="color: rgb(211, 211, 211);" class="fas fa-edit"></i> to open the Whiteboard menu and dynamically make notes on presentations.

| Icon / Key                        | Function                 |
| :-------------------------------- | ------------------------ |
| <i class="fas fa-magic"></i>      | use a laser pointer      |
| <i class="fas fa-eraser"></i>     | use an eraser            |
| <i class="fas fa-pen"></i>        | make notes on slides     |
| <i class="fas fa-undo"></i>       | undo the last action     |
| <i class="fas fa-border-all"></i> | show the background grid |
| <i class="fas fa-plus"></i>       | add a blank slide below  |
| <i class="fas fa-save"></i>       | save whiteboard notes    |

# Save Notes & Drawings

When you save <i class="fas fa-save"></i> slide notes and whiteboard drawings, a new whiteboard file \* will be created in your project directory ending in -annot.json:

## {.small}

```{.yaml}
> project directory
    > audio
    > imgs
    decker.yaml
    example-deck.md
    example-annot.json *
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

# Other Features

Use these keys to try out the other Decker features:

`o` - Slide Overview\
`l` - Laser Pointer\
`b` - Blackout\
`f` - Fullscreen

## Zoom

Double-click on any element in your presentation to zoom in.\
Double-click again to zoom-out.

# Keyboard Shortcut Menu {.columns}

## {.left}

| Key  | Direction |
| ---- | --------- |
| `←`  | left      |
| `→`  | right     |
| `↑`  | up        |
| `↓`  | down      |
| n    | next      |
| p    | previous  |
| home | first     |
| end  | last      |

## {.right}

| Key                 | Command       |
| ------------------- | ------------- |
| b                   | blackout      |
| d                   | drawing       |
| f                   | fullscreen    |
| m                   | menu          |
| o                   | overview      |
| q                   | quiz          |
| s                   | speaker notes |
| w                   | whiteboard    |
| `ctrl` + `shft` + f | search        |

# Configuration Options {#yaml}

Options to configure your presentation are defined in YAML metadata in the `decker.yaml` file. Override these options in your markdown file.

i.e. located at the top of the `*.md` file:

## {.small}

```{.yaml}
---
title: Decker Slide Tool Reference Guide
bibliography: example.bib
csl: chicago-author-date.csl
---
```

# Configuration Options

| Parameter         | Options | Effect                       |
| ----------------- | ------- | ---------------------------- |
| `author`          | String  | Show author on first slide   |
| `date`            | String  | Show date on first slide     |
| `title`           | String  | Show title on first slide    |
| `subtitle`        | String  | Show subtitle on first slide |
| `width`, `height` | numeric | Define aspect ratio          |

# Configuration Options

The options below all accept either "true" or "false".

| Parameter     | Effect                                 |
| ------------- | -------------------------------------- |
| `menu`        | Include menu showing table of contents |
| `progress`    | Include a progress bar                 |
| `slideNumber` | Include slide numbers                  |
| `history`     | Show slides in browser history         |
| `controls`    | Include arrow controls                 |
| `whiteboard`  | Include whiteboard plugin              |
| `chart`       | Include chart plugin                   |

# Configuration Options

| Parameter      | Options           | Effect                          |
| -------------- | ----------------- | ------------------------------- |
| `csl`          | filepath          | Include a citation style (.csl) |
| `bibliography` | filepath          | Include bibliography            |
| `css`          | filepath          | Additional CSS resources        |
| `lang`         | ISO language code | HTML content language           |
| `dir`          | `RTL` or `LTR`    | Text content direction          |

# Decker Commands

Use on the command line after the word `decker` (ie `decker clean`)

| Command   | Function                                                                                                                                                 |
| :-------- | :------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `clean`   | removes the `public` directory and clears cached resource folders with version number older than the current version                                     |
| `decks`   | creates HTML slide decks                                                                                                                                 |
| `example` | copies an example project to the current directory                                                                                                       |
| `html`    | creates all HTML files without opening a server                                                                                                          |
| `info`    | prints information about the current project's directories, files that will be generated and meta data options found in the top level `decker.yaml` file |
| `pdf`     | creates pdf version of all files                                                                                                                         |

# Decker Commands

| Command     | Function                                                                                                                                                                   |
| :---------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `pdf-decks` | creates pdf versions of the html slide decks                                                                                                                               |
| `publish`   | publish generated files to a remote location using `rsync` \*\*                                                                                                            |
| `server`    | like `decker watch` but additionally starts a local web server at the address `localhost:8888` and serves generated HTML files. Changed files are reloaded in the browser. |
| `watch`     | builds HTML versions of all documents and watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `Ctrl C`     |

## {.small}

\*\*_The keys `rsync-destination.host` and `rsync-destination.path` in YAML metadata must specify the publishing destination._
