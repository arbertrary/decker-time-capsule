---
history: True
title: Decker Quiz Overview
---

# Introduction

This slide deck shows how to create simple quizzes with different question types for a self-learning scenario.

Question types:

- Multiple choice questions
- Insert choice questions
- Freetext questions
- Matching questions

# Multiple Choice Question

## Which file format does decker use? {.qmc}

- [ ] .docx
- [ ] .csv
  - Incorrect. Decker does not use comma separated value files.
- [ ] .xml
  - Incorrect.
- [x] .md
  - Correct! Decker uses markdown files with the .md extension.

# Multiple Choice Syntax {.sub}

- add {.qmc} to the level 2 header
- each answer is listed with a hyphen and brackets **- [ ]**
- an X indicates a correct answer **- [X]**
- tooltips are indented furthur

# Multiple Choice Example {.sub}

## {.x-small}

```{.markdown}
## Which file format does decker use? {.qmc}

- [ ] .docx
    - Incorrect. This is the standard format for Microsoft Word documents.
- [ ] .csv
    - Incorrect. Decker does not use comma separated value files.
- [ ] .xml
    - Incorrect.
- [x] .md
    - Correct! Decker uses markdown files with the .md extension.
```

# Insert Choice Question

## The Residence Palace {.qic}

The Würzburg Residence Palace is nicknamed the

- [ ] “Weisser Saal”
  - Incorrect. The Weisser Saal or White Hall in Rococo style was the audience chamber and is dominated by the stucco decorations of Antonio Bossi.
- [x] “Castle above all Castles”
  - Correct! The Residence Palace is one of Europe’s most renowned Baroque castles and has been registered as a UNESCO World Cultural Heritage Site in 1981.
- [ ] “The Imperial Hall”
  - Incorrect. This hall was used to receive visiting dignitaries, including the Emperors-to-be on their voyage to Frankfurt and on the return trip to Vienna.

# Insert Choice Syntax {.sub}

- add {.qic} to the level 2 header
- each answer is listed with a hyphen and brackets **- [ ]**
- an X indicates a correct answer **- [X]**
- tooltips are indented furthur

# Insert Choice Example {.sub}

## {.x-small}

```{.markdown}
## The Residence Palace {.qic}

The Würzburg Residence Palace is nicknamed the

- [ ] “Weisser Saal”
    - Incorrect. The Weisser Saal or White Hall in Rococo style was the audience chamber
      and is dominated by the stucco decorations of Antonio Bossi.
- [x] “Castle above all Castles”
    - Correct! The Residence Palace is one of Europe’s most renowned Baroque castles and
      has been registered as a UNESCO World Cultural Heritage Site in 1981.
- [ ] “The Imperial Hall”
    - Incorrect. This hall was used to receive visiting dignitaries, including the
      Emperors-to-be on their voyage to Frankfurt and on the return trip to Vienna.
```

# Freetext Question

## The Residence Palace {.qft}

In the Würzburg Residence you can find the largest fresco in the world which is 677m² in size. Where is this fresco located?

- ceiling
  - Giovanni Battista Tiepolo, summoned specially from Venice for the purpose, decorated the ceiling of the grand staircase vault in 1752/53 with the largest ceiling fresco ever painted.
- Decke
- Obergrenze

# Freetext Syntax {.sub}

- add {.qft} to the level 2 header
- each _correct_ answer is listed with a hyphen **-**
- tooltips are indented furthur

# Freetext Example {.sub}

## {.x-small}

```
## The Residence Palace {.qft}

In the Würzburg Residence you can find the largest fresco in the world which is 677m²
in size. Where is this fresco located?

- ceiling
  - Giovanni Battista Tiepolo, summoned specially from Venice for the purpose, decorated
  the ceiling of the grand staircase vault in 1752/53 with the largest ceiling fresco
  ever painted.
- Decke
- Obergrenze
```

# Matching Question

## Complete the matches {.qmi}

A
: pair with A

Haskell
: ![](img/haskell.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

!
: $x = {-b \pm \sqrt{b^2-4ac} \over 2a}$

# Matching Syntax {.sub}

- add {.qmi} to the level 2 header
- list answers under match items
- preceed each answer with a colon `:`
- distractor answers are listed with an exclamation point `!`

# Matching Example {.sub}

## {.x-small}

```{.markdown}
## Question: Match the pair {.qmi}

A
: pair with A

Haskell
: ![](img/haskell.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

!
: $x = {-b \pm \sqrt{b^2-4ac} \over 2a}$
```
