---
chalkboard: True
history: True
title: Decker Media Guide
vertical-slides: True
---

# Images

![](img/haskell.png){width="30%"}

##  {.small}

    ![Image Caption](image-file-location){css-formatting}

    ![](img/haskell.png){width="30%"}

# Fullscreen Images

Fullscreen images are added to the slide header. Do not give the slide a title.

    # ![](img/haskell.png)

# ![](img/haskell.png) {.sub data-menu-title="Fullscreen Images Example"}

# Videos {.columns}

##  {.left}

![Controls](movies/jmu-hci-intro.mp4){controls="true" width="450px"}

![Autoplay](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" width="450px"}

##  {.right}

![Start Time](movies/jmu-hci-intro.mp4){controls="true" start="1" width="450px"}

![Looping](movies/jmu-hci-intro.mp4){controls="true" data-autoplay="true" loop="true" width="450px"}

# Video Syntax {.sub}

##  {.x-small}

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

##  {.example}

**YouTube example:** https://www.youtube.com/watchv=<u>qEcmwHRG2Mo</u>\
**YouTube video ID:** qEcmwHRG2Mo

# Fullscreen Videos

Add fullscreen videos to the slide header. Do not include a slide title.

    # ![](movies/jmu-hci-intro.mp4){controls="true"}

# ![](movies/jmu-hci-intro.mp4){.sub data-menu-title="Fullscreen Videos Example" controls="true"}

# Audio {.columns}

##  {.left .example}

![Controls](audio/blackbird.mp3){controls="true"}

##  {.right .example}

![Autoplay](audio/blackbird.mp3){controls="true" data-autoplay="true"}

##  {.small .bottom}

    ![Controls](audio/blackbird.mp3){controls="true"}

    ![Autoplay](audio/blackbird.mp3){controls="true" data-autoplay="true"}

# External Websites

Include external websites using the `{.iframe}` tag.

##  {style="font-size:24px"}

    ![](https://www.uni-wuerzburg.de/){height="20em" .iframe}

# ![](https://www.uni-wuerzburg.de/){.iframe .sub height="20em" data-menu-title="Fullscreen Videos Example"}

# Embed PDF Documents

##  {style="font-size:24px"}

    # ![](http://pandoc.org/MANUAL.pdf)

# ![](http://pandoc.org/MANUAL.pdf){.sub data-menu-title="Embed PDFs Example"}

# Whiteboard

Use the Whiteboard to dynamically make notes on presentations.

##  {style="font-size:24px"}

| Icon / Key                                                           | Function               |
|:---------------------------------------------------------------------|:-----------------------|
| <i class="fas fa-pen"></i>                                           | make notes on slides   |
| <i class="fas fa-eraser"></i>                                        | use an eraser          |
| <i class="fas fa-edit"></i>                                          | open the whiteboard    |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i>             | draw on the whiteboard |
| <i class="fas fa-edit"></i> + <i class="fas fa-pen"></i> + `<ENTER>` | extend the whiteboard  |
| <i class="fas fa-magic"></i>                                         | use a laser pointer    |
| `<del>`                                                              | clear slide            |

# Speaker Notes

Add **`{.notes}`** to a slide header to create notes that appear in the speaker view. The slide is used as the speaker notes for the slide above it. (Press **`s`** to access speaker view.)

##  {.small}

    # Why Gamify? {.notes}

    - Games are among the most powerful motivational tools.
    - Make the non-game experience more rewarding.
    - Motivation has limits. A large leaderboard divide may
      cause the player to abandon the game.

# Why Gamify? {.notes}

-   Games are among the most powerful motivational tools.

-   Make the non-game experience more rewarding

-   Motivation has limits. A large leaderboard divide may cause the player to abandon the game.
