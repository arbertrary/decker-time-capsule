---
title: Charts, Diagrams, Tables
chart: true
---

# Charts from JSON Strings

Include a chart as a JSON string.

For further information see: [https://github.com/rajgoel/reveal.js-plugins/tree/master/chart](https://github.com/rajgoel/reveal.js-plugins/tree/master/chart)

# Bar Chart

```bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```

# Bar Chart with Error Bars

```bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "data": {
        "datasets":[
        {
            "errorBars": {
                "January": { "plus": 15, "minus":  3 },
                "April":   { "plus":  5, "minus": 24 },
                "May":     { "plus":  3, "minus": 14 },
                "August":  { "plus": 10, "minus":  4 }
            }
        },    {
            "errorBars": {
                "February": { "plus": 15, "minus":  3 },
                "March":    { "plus":  5, "minus": 24 },
                "May":      { "plus":  3, "minus": 14 },
                "June":     { "plus": 10, "minus":  4 }
            }
        }  ]
    }
}
-->
```

# Specify Color Schemes

```bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "options": {
        "plugins": {
            "colorschemes": { "scheme": "brewer.SetOne9" }
        }
    }
}
-->
```

# Specify Individual Colors

```bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
  "data": {
    "datasets":[
    {
      "borderColor": "rgba(100,0,0,1.0)",
      "backgroundColor": "rgba(255,0,0,0.7)"
    },
    {
      "borderColor": "rgba(0,100,0,1.0)",
      "backgroundColor": "rgba(0,255,0,0.7)"
    } ]
  }
}
-->
```

# Stacked Bar Chart

```bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "options": {
        "scales": {
            "xAxes": [{ "stacked": true }],
            "yAxes": [{ "stacked": true }]
        }
    }
}
-->
```

# Horizontal Bar Chart

```horizontalBar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```

# Filled Line Chart

```line-chart
January, February, March, April, May, June, July, August, September, October, November, December
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```

# Empty Line Chart

```line-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, -52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0
Derek Jones, 98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0
<!--
{
  "data": {
    "datasets":[
    {
      "backgroundColor": "rgba(255,255,255,0)"
    },
    {
      "backgroundColor": "rgba(255,255,255,0)"
    } ]
  }
}
-->
```

# Radar Chart

```radar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```

# Doughnut Chart

```doughnut-chart
January, February, March, April, May, June
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0
```

# Pie Chart

```pie-chart
1.0, 1.3, 1.7, 2.0, 2.3, 2.7, 3.0, 3.3, 3.7, 4.0, 5.0
Irgendwelche Zahlen, 5, 6, 5, 2, 3, 3, 4, 3, 3, 5, 11
```

# Polar Area Chart

```polarArea-chart
January, February, March, April, May
James Smith, 25.0, 9.0, 18.0, 20.0, 27.0
```

# Graph Diagram with [GraphViz](https://www.graphviz.org/)

```{.dot .render height=500px}
digraph {
    node [style = filled]
    A [fillcolor = red]
    C [fillcolor = green]
    D [fillcolor = blue]
    A -> B
    A -> C
    C -> D
    C -> E
    C -> F
    B -> D
}
```

# Diagrams with Tikz/Latex

```{.tikz .render height=500px}
\begin{tikzpicture}[scale=3,cap=round]
  % Local definitions
  \def\costhirty{0.8660256}

  % Colors
  \colorlet{anglecolor}{green!50!black}
  \colorlet{sincolor}{red}
  \colorlet{tancolor}{orange!80!black}
  \colorlet{coscolor}{blue}

  % Styles
  \tikzstyle{axes}=[]
  \tikzstyle{important line}=[very thick]
  \tikzstyle{information text}=[rounded corners,fill=red!10,inner sep=1ex]

  % The graphic
  \draw[style=help lines,step=0.5cm] (-1.4,-1.4) grid (1.4,1.4);

  \draw (0,0) circle (1cm);

  \begin{scope}[style=axes]
    \draw[->] (-1.5,0) -- (1.5,0) node[right] {$x$};
    \draw[->] (0,-1.5) -- (0,1.5) node[above] {$y$};

    \foreach \x/\xtext in {-1, -.5/-\frac{1}{2}, 1}
      \draw[xshift=\x cm] (0pt,1pt) -- (0pt,-1pt) node[below,fill=white]
            {$\xtext$};

    \foreach \y/\ytext in {-1, -.5/-\frac{1}{2}, .5/\frac{1}{2}, 1}
      \draw[yshift=\y cm] (1pt,0pt) -- (-1pt,0pt) node[left,fill=white]
            {$\ytext$};
  \end{scope}

  \filldraw[fill=green!20,draw=anglecolor] (0,0) -- (3mm,0pt) arc(0:30:3mm);
  \draw (15:2mm) node[anglecolor] {$\alpha$};

  \draw[style=important line,sincolor]
    (30:1cm) -- node[left=1pt,fill=white] {$\sin \alpha$} +(0,-.5);

  \draw[style=important line,coscolor]
    (0,0) -- node[below=2pt,fill=white] {$\cos \alpha$} (\costhirty,0);

  \draw[style=important line,tancolor] (1,0) --
    node [right=1pt,fill=white]
    {
      $\displaystyle \tan \alpha \color{black}=
      \frac{ {\color{sincolor}\sin \alpha} }{\color{coscolor}\cos \alpha}$
    } (intersection of 0,0--30:1cm and 1,0--1,1) coordinate (t);

  \draw (0,0) -- (t);
\end{tikzpicture}
```

```

```
