---
title: Guide to Presenting with Decker
history: true
vertical-slides: true
chalkboard: true
---
    
# General Workflow

1. Begin with an existing project or use `decker example` to generate an example project. 
2. Navigate to your project or the example directory created.
3. Use `decker server` to create html pages and open a local server.
4. In a browser window navigate to `localhost:8888`.
5. Decker uses markdown files ending in `-deck.md`. Create or edit files and see changes in the browser window on file save.
6. When finished shut down the server by pressing `Control C` on the command line.

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
| `publish`   | publish generated files to a remote location using `rsync` **                                                                                            |

## {.small}

***The keys `rsync-destination.host` and `rsync-destination.path` in YAML metadata must specify the publishing destination.*

# Decker Commands

## {.definition .small}

| Command  | Function                                                                                                                                                                        |
| :------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `server` | like `decker watch` but this additionally starts a local web server at the address `localhost:8888` and serves generated HTML files. Changed files are reloaded in the browser. |  | `tutorial` | copies extended examples and tutorial decks to the current directory |
| `watch`  | builds HTML versions of all documents and watches for document changes. Each change to a watched document triggers a rebuild. Watching can be terminated with `Ctrl C`          |

# General Information

## Folder Structure

-   When running, decker creates a `public` folder in the current directory.
-   This folder contains the template and support files needed during
    presentation time as well as the generated `html` and `pdf` files.
    
# Keyboard Shortcut Menu {.grid}

## {.small .left}

| Key          | Direction |
| :----------- | :-------- |
| `←`          | left      |
| `→`          | right     |
| ^`↑`^ ~or~ k | up        |
| ^`↓`^ ~or~ j | down      |
| n ~or~ space | next      |
| p            | previous  |
| home         | first     |
| end          | last      |

##  {.small .center}

| Key    | Command              |
| :----- | :------------------- |
| w      | toggle whiteboard ** |
| l      | toggle laser         |
| d      | toggle drawing       |
| e      | toggle eraser        |
| delete | clear slide          |
| b      | toggle blackout      |
| m      | toggle menu          |


## {.small .right}

| Key  | Command                                |
| :--- | :------------------------------------- |
| s    | speaker notes                          |
| f    | fullscreen                             |
| o    | overview                               |
| ?    | shortcuts                              |
| esc  | escape fullscreen, overview, shortcuts |


## {.small .bottom-left}

***YAML metadata must have chalkboard:true*

# PDF Generation from Command Line

Create PDFs from your HTML presentation slides.

| Command            | Function                                          |
| :----------------- | :------------------------------------------------ |
| `decker pdf`       | creates pdf version of all files                  |
| `decker pdf-decks` | creates pdf versions only of the html slide decks |


# PDF Generation from Browser

**Google Chrome must be installed.** 

1. start `decker server` 
2. add **?print-pdf** to the end of the URL address **
3. open the browser's print menu (CTRL/CMD+p)
4. change the Destination to **Save as PDF**
5. change the Layout to landscape and Margins to none
6. click to Enable Background Graphics
7. click **Save**


## {.x-small} 

    ** http://localhost:8888/example-deck.html?print-pdf

