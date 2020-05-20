---
bibliography: "example.bib"
csl: "chicago-author-date.csl"
history: True
table-of-contents: True
title: Decker Syntax Guide
vertical-slides: True
---

# About

This syntax guide will highlight some of the main styling features of Decker and provide examples on how to use each feature.

Visit <http://pandoc.org> for additional information on Pandoc-Markdown text formatting.

# Slide Headers

## Heading 2 (h2) {.example}

### Heading 3 (h3) {.example}

#### Heading 4 (h4) {.example}

##

Create new slides with a single pound sign (Heading 1)

    # Heading 1 (h1) *new slide created
    ## Heading 2 (h2)
    ### Heading 3 (h3)
    #### Heading 4 (h4)

# Text Emphasis

## {.split}

###

## {.example}

**This is bold text**\
**This is bold text**\
_This is italic text_\
_This is italic text_\
~~Strikethrough~~\
<u>underline</u>\
H~2~O is a liquid.\
2^3^ equals 8.

##

    **This is bold text**
    __This is bold text__
    *This is italic text*
    _This is italic text_
    ~~Strikethrough~~
    <u>underline</u>
    ~subscript~
    ^superscript^

# Text Size {.columns}

## {.left}

## xx-small Text {.xx-small}

## x-small Text {.x-small}

## small Text {.small}

## medium Text {.medium}

## large Text {.large}

## x-large Text {.x-large}

## xx-large Text {.xx-large}

## {.x-small .right}

    ## xx-small Text {.xx-small}

    ## x-small Text {.x-small}

    ## small Text {.small}

    ## medium Text {.medium}

    ## large Text {.large}

    ## x-large Text {.x-large}

    ## xx-large Text {.xx-large}

# Highlight Text

## {.small .example}

This splendid palace of the Prince-Bishops is one of the finest secular Baroque buildings in Germany. Built between <mark>1720 and 1744</mark>, one of the most notable features of this UNESCO World Heritage Site is its spectacular monumental staircase hall with its huge fresco by Tiepolo. All told, some <mark>40 rooms are open to visitors, including the White Hall, with its Rococo stucco work, the sumptuously decorated Imperial Hall, the Hall of Mirrors, and the beautiful Court Church</mark>. Although heavily damaged in WWII, much of the building has now been restored to its former glory, a process that took over 42 years to complete.

##

    Built between <mark>1720 and 1744</mark>

# Emojis

Decker supports the use of emojis in your presentation.

- Powerpoint: 😢
- LaTeX-Beamer: 😊
- Decker: 😍

# Vertical Slides

Use the `{.sub}` tag to add a slide below. Use the `↓` key or press the spacebar to navigate downward.

## {.small}

    # Würzburger Residenz {.sub}

    In contrast to the Münchner Residenz, which grew over half of a
    millennium and therefore contains stylistically diverse epochs,
    the Würzburg Residenz was built from scratch in just under a
    generation with brief interruptions.

# Würzburger Residenz {.sub data-menu-title="Vertical Slides Example"}

In contrast to the Münchner Residenz, which grew over half a millennium and therefore contains stylistically diverse epochs, the Würzburg Residenz was built from scratch in just under a generation with brief interruptions.

# Multicolumn Slides {.columns}

## Würzburger Residenz {.top}

Known for its extravagant artistic design and Baroque architecture

## Alte Mainbrücke {.left}

## Festung Marienberg {.center}

## Marienkapelle {.right}

## Dom St. Kilian {.bottom}

Notable for its beautifully restored interior

# Multicolumn Slides Syntax {.sub}

Use the `{.columns}` tag to create columns. Identify each block with a location tag.

## {.x-small}

    # Multicolumn Slides {.columns}

    ## Würzburger Residenz {.top}

    Known for its extravagant artistic design and Baroque architecture

    ## Alte Mainbrücke {.left}

    ## Festung Marienberg {.center}

    ## Marienkapelle {.right}

    ## Dom St. Kilian {.bottom}

    Notable for its beautifully restored interior

# Column Widths

## this left column has a width of 70% {.column width="70%"}

## this right column has a width of 25% {.column width="25%"}

## {.small}

    ## this left column has a width of 70% {.column width=70%}

    ## this right column has a width of 25% {.column width=25%}

# Grid Layout {.grid}

## One {.top-left .example}

## Two {.top .example}

## Three {.top-right .example}

## Four {.left .example}

## Five {.center .example}

## Six {.right .example}

## Seven {.bottom-left .example}

## Eight {.bottom .example}

## Nine {.bottom-right .example}

# Grid Layout Syntax {.sub .columns}

## {.top}

Use the `{.grid}` tag to partition the slide into a grid. Identify each block with a location tag.

## {.x-small .left}

    # Pick a Number {.grid}

    ## One {.top-left}

    ## Two {.top}

    ## Three {.top-right}

    ## Four {.left}

## {.x-small .right}

    ## Five {.center}

    ## Six {.right}

    ## Seven {.bottom-left}

    ## Eight {.bottom}

    ## Nine {.bottom-right}

# Highlight Blocks

## Alert Block {.alert .split}

- Alert Text

## Question Block {.question}

- Question text

## Answer Block {.answer}

- Answer text

## Definition Block {.definition}

- Definition text

## Observation Block {.observation}

- Observation text

## Example Block {.example}

- Example text

## Equation Block {.equation}

- Equation text

## Note Block {.note}

- Note text

# Highlight Blocks Syntax {.columns .sub}

## {.x-small .left}

    ## Alert Block {.alert}

    -   Alert Text

    ## Question Block {.question}

    -   Question text

    ## Answer Block {.answer}

    -   Answer text

    ## Definition Block {.definition}

    -   Definition text

## {.x-small .right}

    ## Observation Block {.observation}

    -   Observation text

    ## Example Block {.example}

    -   Example text

    ## Equation Block {.equation}

    -   Equation text

    ## Note Block {.note}

    -   Note text

# Verbatim Code Blocks {.columns}

## {.top}

Surround text with **\~\~\~** or **\`\`\`** or indent each line by four spaces to treat text as verbatim.

## {.right}

    ~~~
    | Key  | Direction |
    | :--- | :-------- |
    | `←`  | left      |
    | `→`  | right     |
    | `↑`  | up        |
    | `↓`  | down      |
    ~~~

## {.left}

##

##

##

| Key | Direction |
| :-- | :-------- |
| `←` | left      |
| `→` | right     |
| `↑` | up        |
| `↓` | down      |

# Block Quotes

> This is a block quote.
>
> > A block quote within a block quote.

Preceed each line with **\>** to quote a block of text:

    > This is a block quote.
    >
    > > A block quote within a block quote.

# Mathematics {.columns}

## {.top}

- Enclose math within a line with **\$**
- Enclose a block of math with **\$\$**

## {.left}

## {.example}

To $\infty$ and beyond!

## {.example}

$$ e = mc ^ 2 $$

## {.example}

$$ \lim_{x \to \infty} \exp(-x) = 0 $$

## {.right .medium}

```{.markdown}
To $\infty$ and beyond!
```

```{.markdown}
$$ e = mc ^ 2 $$
```

```{.markdown}
$$ \lim_{x \to \infty} \exp(-x) = 0 $$
```

# Java Syntax

Use the `{.java}` tag to highlight Java code.

```{.java}
String s = "Java highlighting syntax";
System.out.println (s);
```

    ~~~{.java}
    String s = "Java highlighting syntax";
    System.out.println (s);
    ~~~

# Javascript Syntax

Use the `{.javascript}` tag to highlight Javascript code.

```{.javascript}
var s = "JavaScript syntax highlighting";
alert (s);
```

    ~~~{.javascript}
    var s = "JavaScript syntax highlighting";
    alert (s);
    ~~~

# Inverse Colors {.inverse background-color="black"}

## Color Scheme for Dark Images

Add the `{.inverse}` tag to a slide header and add `background-color="black"`.

## Definition Box {.fragment .definition}

Even colored boxes look ok.

# Lists {#lists}

## Ordered Lists {.split}

1.  bread
2.  milk
3.  sugar
4.  flour

##

    1.  bread
    2.  milk
    3.  sugar
    4.  flour

## Enumerated Lists

- Take out trash
- Vaccuum
  - Clean bedroom
- Wash dishes

##

     -  Take out trash
     -  Vaccuum
        - Clean bedroom
     -  Wash dishes

# Sequential Lists

(1) Salman Rushdie, _The Ground beneath Her Feet_ (New York: Henry Holt, 1999), 25.

(2) Bob Stewart, "Wag of the Tail: Reflecting on Pet Ownership," in _Enriching Our Lives with Animals_, ed. John Jaimeson, Tony Bannerman and Selena Wong (Toronto, ON: Petlove Press, 2007),100.

Additional sources:

(3) Elliot Antokoletz, _Musical Symbolism in the Operas of Debussy and Bartok_ (New York: Oxford University Press, 2008), doi:10.1093/acprof:oso/9780195365825.001.0001.

# Sequential Lists Syntax {.sub}

Use the (@) symbol to automatically number items in a list.\
Numbered examples do not need to be in a single list.

## {.x-small}

    (@)  Salman Rushdie, *The Ground beneath Her Feet* (New York: Henry Holt, 1999), 25.

    (@)  Bob Stewart, "Wag of the Tail: Reflecting on Pet Ownership," in *Enriching Our
      Lives with Animals*, ed. John Jaimeson, Tony Bannerman and Selena Wong
      (Toronto, ON: Petlove Press, 2007),100.

    Additional sources:

    (@)  Elliot Antokoletz, *Musical Symbolism in the Operas of Debussy and Bartok*
      (New York: Oxford University Press, 2008),
      doi:10.1093/acprof:oso/9780195365825.001.0001.

# Fragments

Fragment blocks of content will appear only if you continue clicking.

## Question {.question}

This 2.13 meter tall Würzburger, nicknamed the 'German Wunderkind' made waves in America as the first European to start in an NBA All-Star game.

## Answer {.fragment .answer}

Dirk Nowitzki

# Fragments Syntax {.sub}

## {.small}

    ##  Question {.question}

    This 2.13 meter tall Würzburger, nicknamed the ...

    ##  Answer {.fragment .answer}

    Dirk Nowitzki

# Fragmented Lists {.columns}

## {.top}

Create lists with items that appear sequentially by prepending **\>** to the first item in the list.

## {.right}

    ## Programming Languages

    > - Python
    - Javascript
    - Java
    - C#
    - C++
    - PHP
    - Scala

## {.left}

##

## Programming Languages {.example}

> - Python
> - Javascript
> - Java
> - C\#
> - C++
> - PHP
> - Scala

# Links

## {.example}

Visit <http://pandoc.org> for additional information.\
Read more about building [lists](#lists) in Decker.

## {.small}

    Visit [http://pandoc.org](http://pandoc.org) for additional
    information.

    Read more about building [lists](#lists) in Decker.

# Links Syntax {.sub}

    [text-to-be-displayed](https://url-of-website)
    [text-to-be-displayed](#slide-id)

_Note:_ Slide IDs are entered on the slide header (h1) as follows:

    # Slide Title {#slide-id}

# Tables

## {.example}

| Week | Topic                            |    Reading     | Book    |
| ---: | :------------------------------- | :------------: | ------- |
|    1 | Course Introduction              |    Chapt. 1    | Physics |
|    2 | Inertia, Equilibrium, Kinematics | Chapt. 2, 3, 4 | Physics |
|    3 | Vectors, Momentum, Energy        |   Chapt. 5-8   | Physics |

: Assignment List

# Tables Syntax {.sub}

Use **`|`** and **`-`** to create tables. Align text using **`:`** on the left, right, or on both sides of the hyphens in the header row.

## {.small}

    Table: Assignment List

    | Week | Topic                            |  Reading   | Book    |
    | ---: | :------------------------------- | :--------: | ------- |
    |    1 | Course Introduction              |  Chapt. 1  | Physics |
    |    2 | Inertia, Equilibrium, Kinematics | Chapt. 2-3 | Physics |
    |    3 | Vectors, Momentum, Energy        | Chapt. 4-7 | Physics |

# Images

![](img/haskell.png){width="30%"}

## {.small}

    ![Image Caption](image-file-location){css-formatting}

    ![](img/haskell.png){width="30%"}

# Fullscreen Images

Fullscreen images are added to the slide header. Do not give the slide a title.

    # ![](img/haskell.png)

# ![](img/haskell.png){.sub data-menu-title="Fullscreen Images Example"}

# Videos {.columns}

## {.left}

![Controls](movies/jmu-hci-intro.mp4){controls="true" width="450px"}

![Autoplay](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" width="450px"}

## {.right}

![Start Time](movies/jmu-hci-intro.mp4){controls="true" start="1" width="450px"}

![Looping](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" loop="true" width="450px"}

# Video Syntax {.sub}

## {.x-small}

    ![Controls](movies/jmu-hci-intro.mp4){controls="true"}

    ![Autoplay](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true"}

    ![Start Time](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" start="1"}

    ![Looping](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" loop="true"}

# External Videos

![](youtube://qEcmwHRG2Mo){width="75%" start="10"}

    ![](youtube://qEcmwHRG2Mo){width="75%" start="10"}

# External Videos Syntax {.sub}

Include YouTube and Vimeo videos or Twitch channels in presentations. Replace `service` with `youtube`, `vimeo` or `twitch` and add the video id or twitch channel name (replaces `video-id`). The video ID is usually found in the URL.

    ![](service://video-id){css-formatting}

## {.example}

**YouTube example:** https://www.youtube.com/watchv=<u>qEcmwHRG2Mo</u>\
**YouTube video ID:** qEcmwHRG2Mo

# Fullscreen Videos

Add fullscreen videos to the slide header. Do not include a slide title.

    # ![](movies/jmu-hci-intro.mp4){controls="true"}

# ![](movies/jmu-hci-intro.mp4){.sub data-menu-title="Fullscreen Videos Example" controls="true"}

# Audio {.columns}

## {.left .example}

![Controls](audio/blackbird.mp3){controls="true"}

## {.right .example}

![Autoplay](audio/blackbird.mp3){controls="true" data-autoplay="true"}

## {.small .bottom}

    ![Controls](audio/blackbird.mp3){controls="true"}

    ![Autoplay](audio/blackbird.mp3){controls="true" data-autoplay="true"}

# External Websites

Include external websites using the `{.iframe}` tag.

## {style="font-size:24px"}

    ![](https://www.uni-wuerzburg.de/){height="20em" .iframe}

# ![](https://www.uni-wuerzburg.de/){.iframe .sub height="20em" data-menu-title="Fullscreen Videos Example"}

# Embed PDF Documents

## {style="font-size:24px"}

    # ![](http://pandoc.org/MANUAL.pdf)

# ![](http://pandoc.org/MANUAL.pdf){.sub data-menu-title="Embed PDF Example"}

# Whiteboard

Use the Whiteboard to dynamically make notes on presentations.

## {style="font-size:24px"}

| Icon / Key                                                           | Function               |
| :------------------------------------------------------------------- | :--------------------- |
| <i class="fas fa-pen"></i>                                           | make notes on slides   |
| <i class="fas fa-eraser"></i>                                        | use an eraser          |
| <i class="fas fa-edit"></i>                                          | open the whiteboard    |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i>             | draw on the whiteboard |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i> + `<ENTER>` | extend the whiteboard  |
| <i class="fas fa-magic"></i>                                         | use a laser pointer    |
| `<del>`                                                              | clear slide            |

# Speaker Notes

Add **`{.notes}`** to a slide header to create notes that appear in the speaker view. The slide is used as the speaker notes for the slide above it. (Press **`s`** to access speaker view.)

## {.small}

    # Why Gamify? {.notes}

    - Games are among the most powerful motivational tools.
    - Make the non-game experience more rewarding.
    - Motivation has limits. A large leaderboard divide may
      cause the player to abandon the game.

# Why Gamify? {.notes}

- Games are among the most powerful motivational tools.

- Make the non-game experience more rewarding

- Motivation has limits. A large leaderboard divide may cause the player to abandon the game.

# Citations

Add citations to your slide deck. Be sure to include a `csl` and a `bib` file in your YAML header.

## {.example}

Have you heard about Space Tentacles [@zimmerer2018space]. According to @zimmerer2018space it is a nice idea.

# Citations Syntax {.sub}

## {.small}

    ---
    bibliography: 'example.bib'
    csl: 'chicago-author-date.csl'
    ---

    # Space Tentacles

    Have you heard about Space Tentacles [@zimmerer2018space].
    According to @zimmerer2018space it is a nice idea.

# References

References are automatically addded to your last slide when you include a `csl` and `bib` file in your YAML header.

    ---
    bibliography: 'example.bib'
    csl: 'chicago-author-date.csl'
    ---
