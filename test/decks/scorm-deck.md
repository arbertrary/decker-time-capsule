---
graded: True
scorm: True
---

# SCORM Testing Deck

To create a SCORM package, enter scorm:true in the decker.yaml file of the project. To tally a grade for the quiz, enter graded:true in decker.yaml and assign point values to each question. Use the command decker scorm to create a scorm package to upload to Moodle.

# Question 1 Multiple Choice {.question points="14"}

Which of the following is the healthiest drink?

-   { } soda
-   { } coffee
-   {X} water

# Question 2 Blanktext {.question points="20"}

{blanktext} Health Rules
:   {!Black} is the first color in the German flag. The middle color is { \|!red\|blue\|yellow\|white\|}.

# Question 3 Freetext {.question points="23"}

-   {?} What is the capital of Germany?
-   {!} Berlin

# Question 4 Matching {.question points="24"}

{match} The third president of the United States was:
:   Thomas Jefferson

{match} The second president of the United States was:
:   John Adams
