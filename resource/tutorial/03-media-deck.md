---
title: Decker Media Guide
whiteboard: "03-media-annot.json"
vertical-slides: "true"
---

# Images {.columns}

## {.right}

![Haskell Logo](img/haskell.png){width="30%"}

## {.small .bottom}

```{.yaml}
![Image Caption](image-file-location){css-formatting}

![Haskell Logo](img/haskell.png){width="30%"}
```

# Fullscreen Images

Fullscreen images are added to the slide header. Do not give the slide a title.

## {.small}

```{.markdown}
# ![](img/haskell.png)
```

# ![](img/haskell.png) {.sub data-menu-title="Fullscreen Images Example"}

# Videos {.columns}

## {.left}

![Controls](movies/jmu-hci-intro.mp4){.controls width="450px"}

![Autoplay](movies/jmu-hci-intro.mp4){.controls .autoplay width="450px"}

## {.right}

![Start Time](movies/jmu-hci-intro.mp4){.controls start="1" width="450px"}

![Looping](movies/jmu-hci-intro.mp4){.controls .autoplay .loop width="450px"}

# Video Syntax {.sub}

## {.small}

```{.yaml}
![Controls](movies/jmu-hci-intro.mp4){.controls}

![Autoplay](movies/jmu-hci-intro.mp4){.controls .autoplay}

![Start Time](movies/jmu-hci-intro.mp4){.controls start="1"}

![Looping](movies/jmu-hci-intro.mp4){.controls .autoplay .loop}
```

# External Videos

![Red-winged Blackbird](vimeo:20027678){width="70%" start="8"}

## {.small}

```{.yaml}
![Vimeo](vimeo:20027678){width="85%" start="8"}
```

# External Videos Syntax

## {.small}

```{.yaml}
![caption](service:video-id){css-formatting}
```

##

Replace `service` with `youtube`, `vimeo` or `twitch`. Replace `video-id` with the actual video id or twitch channel name, usually found in the URL:

## {.example}

**YouTube:** https://youtu.be/<u>qEcmwHRG2Mo</u>\
**Vimeo:** https://vimeo.com/<u>9698387</u>\
**Twitch:** https://www.twitch.tv/<u>ieeevr2020_great_room_1</u>

# YouTube Video

![HCI Würzburg](youtube:qEcmwHRG2Mo){width=90%}

# Fullscreen Videos

Add fullscreen videos to the slide header. Do not include a slide title.

## {.small}

```{.markdown}
# ![](movies/jmu-hci-intro.mp4){.controls}
```

# ![](movies/jmu-hci-intro.mp4) {.sub .controls data-menu-title="Fullscreen Videos Example"}

# Audio {.columns}

## {.left .example}

![Controls & Loop](audio/blackbird.mp3){.controls .loop}

## {.right .example}

![Autoplay & Muted](audio/blackbird.mp3){.controls .autoplay .muted}

## {.small .bottom}

```{.yaml}
![Controls](audio/blackbird.mp3){.controls .loop}

![Autoplay & Muted](audio/blackbird.mp3){.controls .autoplay .muted}
```

# External Websites & PDFs

Use the `{.iframe}` tag to embed external websites and PDF Documents.

## {.small}

```{.markdown}
# ![](https://www.uni-wuerzburg.de/){.iframe}

# ![](http://pandoc.org/MANUAL.pdf){.iframe}
```

# ![](https://www.uni-wuerzburg.de/){datamenu-title="External Websites Example" .iframe .sub }

# ![](http://pandoc.org/MANUAL.pdf){.iframe .sub data-menu-title="Embed PDFs Example"}

# Virtual Tables

$$
\begin{eqnarray*}
a &=& b \\
a^2 &=& ab \\
2a^2 &=& a^2 + ab \\
2a^2-2ab &=& a^2 - ab \\
2a(a-b) &=& a (a-b) \\
2a &=& a \\
2 &=& 1
\end{eqnarray*}
$$

# Virtual Tables Syntax {.sub}

## {.small}

```{.markdown}
$$
\begin{eqnarray*}
a &=& b \\
a^2 &=& ab \\
2a^2 &=& a^2 + ab \\
2a^2-2ab &=& a^2 - ab \\
2a(a-b) &=& a (a-b) \\
2a &=& a \\
2 &=& 1
\end{eqnarray*}
$$
```
