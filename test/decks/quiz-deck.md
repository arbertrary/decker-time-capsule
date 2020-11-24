---
title: New Quiz Syntax Test deck
lang: de
---

# Overview
  
## Add quiz type to level 2 header: 

```.markdown
## Question 1 {.qmc}
```

## {.small}

- Multiple Choice: `.qmc, .quiz-mc, .quiz-multiple-choice`  
- Matching: `.qmi, .quiz-mi, .quiz-match-items`  
- Insert: `.qic, .quiz-ic, .quiz-insert-choices`  
- Free-text: `.qft, .quiz-ft, .quiz-free-text`  

## Styling

To switch all questions to plain style, add to metadata:

```.yaml
quiz:
  style: plain
```

Or to style individual questions, add `{.plain}`

# Multiple Choice 1 - Fancy

## {.quiz-mc}

All choices get an optional comment to be potentially shown with the result. 


- [X] a
    - So ist das. 
    - test
- [ ] b
    - So nicht. 
    - Tooltip2
- [ ] c
    - So auch nicht

```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```


# Multiple Choice 2 - Plain

## {.quiz-mc .plain}

Choices appear in a column. Click the circle to the left of the choice to select. Choices are corrected immediately. A green check will appear in the circle for correct responses, and a red x for incorrect responses. Hover over the circle to see a tooltip. All choices get an optional comment to be potentially shown with the result. 


- [X] a
    - So ist das. 
    - test
- [ ] b
    - So nicht. 
    - Tooltip2
- [ ] c
    - So auch nicht

```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Multiple Choice 3 - Fancy {.columns}


## {.top}
Given is a light source of radiance 0.5 that illuminates a surface of diffuse reflectivity coefficient 1. Calculate the reflected diffuse energy according to the Phong Lighting Model. The constellation is as shown in the image below.

## {.right}
Trigonometric functions

|     | $0^\circ$ | $30^\circ$           | $45^\circ$           | $60^\circ$           | $90^\circ$ |
| --- | --------- | -------------------- | -------------------- | -------------------- | ---------- |
| sin | 0         | $\frac{1}{2}$        | $\frac{\sqrt{2}}{2}$ | $\frac{\sqrt{3}}{2}$ | 1          |
| cos | 1         | $\frac{\sqrt{3}}{2}$ | $\frac{\sqrt{2}}{2}$ | $\frac{1}{2}$        | 0          |


## {.qmc .inline}

- [ ] $\frac{\sqrt{3}}{4}$
  - $1\cdot0.5\cdot\cos(\vec s\cdot \vec n) = 1\cdot0.5\cdot\cos(30^\circ)$
- [X] $\frac{\sqrt{3}}{2}$
- [ ] $\frac{1}{2}$
  - $1\cdot0.5\cdot\cos(\vec s\cdot \vec n)$  
    $= 1\cdot0.5\cdot\cos(30^\circ)$
- [ ] $\frac{1}{4}$
  - $1\cdot0.5\cdot\cos(\vec s\cdot \vec n)$  
    $= 1\cdot0.5\cdot\cos(30^\circ)$



```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Multiple Choice 4 - Plain Inline 

## What does the transpose of the following matrix look like? {.qmc .inline .plain}

\begin{equation*}
\begin{bmatrix}
  1 & 2\\
  3 & 2\\
\end{bmatrix}
\end{equation*}

* [ ] $\begin{bmatrix}
  1 & 2\\
  3 & 2\\
\end{bmatrix}$
* [ ] $\begin{bmatrix}
  2 & 2\\
  3 & 1\\
\end{bmatrix}$
* [ ] $\begin{bmatrix}
  2 & 3\\
  2 & 1\\
\end{bmatrix}$
* [X] $\begin{bmatrix}
  1 & 3\\
  2 & 2\\
\end{bmatrix}$

```yaml
Points: 5
LectureId: cg1
TopicId: yeah
Difficulty: Easy
```

# Matching

Plain matchinq questions display a question/term, followed by a select box and a list of possible answers. Click the select box and then click answers as desired. Check marks will appear. Click outside of the select box to close. Select additional or deselect selected responses as desired. After clicking the solution button, answers will turn green or red to display correctness. Change answers as desired and click solve again. A grey box surrounds the matching questions and responses to ensure that all are shown. The solution button is below this.

Fancy matching questions feature drag and drop questions. Drag answers to question boxes and click solve. These questions should not flow outside of the answer box.

# Matching 1 - Fancy

Drag response to answers. Switch responses even after solved. A scrollbar appears when too many items are dragged to an answer. The solution button never goes beyond the slide. Be sure that responses do not flow outside of the answer box.

## {.qmi}

Gravity
: $(0, -k\,m, 0)\T$

Damping
: $-k \, \dot{\vec{x}}$

Collisions
: $k \, d \, \vec{n}$

Inertia
: $m \, \ddot{\vec{x}}$

Springs
: $-k \, \left( \norm{\vec{x}_0 - \vec{x}_1} - L \right) \, \frac{ \vec{x}_0 - \vec{x}_1 }{\norm{\vec{x}_0 - \vec{x}_1}}$

!
: Distractor A

!
: Distractor B

```yaml
lang: en
```

# Matching 2 - Plain

## {.qmi .plain}

Gravity
: $(0, -k\,m, 0)\T$

Damping
: $-k \, \dot{\vec{x}}$

Collisions
: $k \, d \, \vec{n}$

Inertia
: $m \, \ddot{\vec{x}}$

Springs
: $-k \, \left( \norm{\vec{x}_0 - \vec{x}_1} - L \right) \, \frac{ \vec{x}_0 - \vec{x}_1 }{\norm{\vec{x}_0 - \vec{x}_1}}$

!
: Distractor A

!
: Distractor B

```yaml
lang: en
```

# Matching 3 - Fancy

## {.quiz-mi} 

Drag the elements to create correct pairings

A
: drag to A


Image 
: ![](include/06-metal.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

C
: $\Leftarrow$ C

Empty
: !

!
: Distractor A

!
: Distractor B

```yaml
score: 5
category: CG
lectureId: cg1
topic: yeah
```

# Matching 4 - Plain

## {.quiz-mi .plain} 

Drag the elements to create correct pairings

A
: drag to A
: also drag to A


Image 
: ![](include/06-metal.png)

B
: drag to B

decker
: [decker](http://go.uniwue.de/decker)

C
: $\Leftarrow$ C

Empty
: !

!
: Distractor A

!
: Distractor B

```yaml
score: 5
category: CG
lectureId: cg1
topic: yeah
```


# Matching 5 - Plain

## Question text {.qmi .plain}

Das unmarkierte Quadrat unten links im Bild ist mit einer Textur (blauer
Pfeil, oranger Pfeil) versehen. Die Texturkoordinaten wurden für dieses
Quadrat *nicht* transformiert. 

Bild 1
: Rz(180)

Bild 2
: Sxy(1, -1)

Bild 3
: Sxy(3, 3)

Bild 4
: Sxy(0.5, 0.5) Txy(0.5, 0.5)

Bild 5
: Rz(-45)Txy(0.5, 0.5)

!
: Sxy(-1,1)

!
: Sxy(0.3, 0.3)

!
: Sxy(-1,1) Txy(0.5, 0.5)

!
: Rz(45)

Bild 6
: ! 

# Matching 6 - Fancy

## Question text {.qmi}

Was gehört zusammen? Verbinden Sie.

bislang
: bis jetzt, bis heute

zwischenzeitlich
: inzwischen, in der Zwischenzeit

daher
: deshalb, aus diesem Grund

auffordern
: (dringend) bitten, an jemand appellieren

offen
: unbezahlt, fällig

!
: jemand etwas tun lassen 


# Matching 7 - Fancy

##  {.qmi}

Below there are some examples of typical machine learning problems. Assign the examples to the better suited learning approach : 

**Supervised Learning**
:   House Prices
:   Cat or Dog? (Image Classification)
:   Weather Forecast
:   Mood Estimation

**Unsupervised Learning**
:   Customer Grouping
:   Dimensionality Reduction 
:   Outlier detection


# Blanktext/Inserting MC

Choices are shown as drop-down lists embedded inside the text. Confirm tooltips display on hover. Questions are corrected immediately, displaying red or green based on correctness. Retry is allowed. Hover over the select box to display the tooltip below the question.

# Insert 1 - Fancy

First (Scala) has no tooltip.

## {.quiz-ic} 

Decker is a software built using  


- [ ] Scala
- [X] Haskell
    - Due to Pandoc.
- [ ] Java
    - Surely not.
- [ ] Ruby 
    - hm no


and builds upon the tool

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

.

```yaml
Points: 5
```

# Insert 2 - Plain

Appears the same as Fancy.
 
## {.quiz-ic .plain} 

Decker is a software built using  


- [ ] Scala
    - Unfortunately not.
- [X] Haskell
    - Due to Pandoc.
- [ ] Java
    - Surely not.
- [ ] Ruby 
    - hm no


and builds upon the tool

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

.

```yaml
Points: 5
```


# Free Text

These questions now include multiple versions of correct answers with additional optional comments (as before). Hence the Syntax is slightly different in its compact representation. 

# Free Text 1 - Fancy

## {.quiz-ft} 

Das Ergebnis von $2*2=~?$ ist?

- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL? 

```yaml
Points: 5
```

# Free Text 2 - Plain

Type the answer and push Enter to correct the question.  

## {.qft .plain}

What's the first letter in the alphabet?

- [X] A
  - The modern form of the capital letter A evolved from a the Latin script, a transformation of Greek script, which is in turn a transformation of the Phoenician script.
- [ ] B
  - The letter B isn't used in any numbers until billion.
- [ ] C
  - "C" comes from the same letter as "G". The Semites named it gimel. The sign is possibly adapted from an Egyptian hieroglyph for a staff sling, which may have been the meaning of the name gimel. Another possibility is that it depicted a camel, the Semitic name for which was gamal. Barry B. Powell, a specialist in the history of writing, states "It is hard to imagine how gimel = "camel" can be derived from the picture of a camel (it may show his hump, or his head and neck!)".[2]
  In the Etruscan language, plosive consonants had no contrastive voicing, so the Greek 'Γ' (Gamma) was adopted into the Etruscan alphabet to represent /k/. Already in the Western Greek alphabet, Gamma first took a 'Early Etruscan C.gif' form in Early Etruscan, then 'Classical Etruscan C.gif' in Classical Etruscan. In Latin it eventually took the 'c' form in Classical Latin. In the earliest Latin inscriptions, the letters 'c k q' were used to represent the sounds /k/ and /ɡ/ (which were not differentiated in writing). Of these, 'q' was used to represent /k/ or /ɡ/ before a rounded vowel, 'k' before 'a', and 'c' elsewhere.[3] During the 3rd century BC, a modified character was introduced for /ɡ/, and 'c' itself was retained for /k/. The use of 'c' (and its variant 'g') replaced most usages of 'k' and 'q'. Hence, in the classical period and after, 'g' was treated as the equivalent of Greek gamma, and 'c' as the equivalent of kappa; this shows in the romanization of Greek words, as in 'ΚΑΔΜΟΣ', 'ΚΥΡΟΣ', and 'ΦΩΚΙΣ' came into Latin as 'cadmvs', 'cyrvs' and 'phocis', respectively. 
  
  Other alphabets have letters homoglyphic to 'c' but not analogous in use and derivation, like the Cyrillic letter Es (С, с) which derives from the lunate sigma, named due to its resemblance to the crescent moon. 

## {.qft .plain}

What's the fourth letter?

- [ ] C
    - "C" comes from the same letter as "G". The Semites named it gimel. The sign is possibly adapted from an Egyptian hieroglyph for a staff sling, which may have been the meaning of the name gimel. 
- [X] D
    - The letter "D" has retained the fourth place in the alphabet from the earliest point at which it appears in history. It corresponds to Semitic daleth and Greek delta (Δ). The form is thought to derive from an early pictograph, possibly Egyptian, indicating the folding door of a tent.

```yaml
Points: 5
```

# Free text alternative

This alternate forms allows you to define free-text questions as lists without brackets. All answers are correct.

# Free Text Alternative 1 - Fancy

## {.quiz-ft} 

Das Ergebnis von $2*2=~?$ ist?


- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL, no not really?

```yaml
Points: 5
```

# Free Text Alternative 2 - Plain

## {.quiz-ft .plain} 

Das Ergebnis von $2*2=~?$ ist?

- 4
    - Die perfekte Lösung 
- vier
    - Auch ok 
- four
    - Deutscher Studiengang
- fier
    - RTL, no not really?

```yaml
Points:
```

# fenced divs syntax

::: qft

Das Ergebnis von $2*2=~?$ ist?


- [X] 4
    - Die perfekte Lösung 
- [X] vier
    - Auch ok 
- [X] four
    - Deutscher Studiengang
- [ ] fier
    - RTL, no not really?
- [ ] *
    - ganz falsch, so oder so?
:::


#

::: qic

Decker is a software built using  


- [ ] Scala
    - Unfortunately not.
- [X] Haskell
    - Due to Pandoc.
- [ ] Java
    - Surely not.
- [ ] Ruby 
    - hm no


and builds upon the tool

- [X] Pandoc
  - See previous tooltip.
- [ ] PowerPoint

.

:::