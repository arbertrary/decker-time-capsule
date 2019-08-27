---
title: Chart Deck
subtitle: Usage of the Chart Plugin
chart: true
---


# Charts from {.json style="font-size:x-small"} Strings

A chart can be included in a slide by adding a canvas element to the `*-deck.md` file  with the data-chart attribute set to the desired chart type.

The chart can be configured within the canvas body by a {.json style="font-size:x-small"} string embedded into an HTML comment.

For further information see: [https://github.com/rajgoel/reveal.js-plugins/tree/master/chart](https://github.com/rajgoel/reveal.js-plugins/tree/master/chart)

# Filled Line Chart

```{.json style="font-size:x-small"}
<canvas id=“filled_line" data-chart="line">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[    
    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "right" },
    "responsive": true
  }
}
-->
</canvas>
```

# Filled Line Chart {.sub}

<canvas id=“filled_line" data-chart="line">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[    
    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "right" },
    "responsive": true
  }
}
-->
</canvas>

# Empty Line Chart

```{.json style="font-size:x-small"}
<canvas id=“empty_line" data-chart="line">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
    {
      "data": [-52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0],
      "label": "James Smith",
      "borderColor": "rgba(255,105,180,1)",
      "backgroundColor": "rgba(255,255,255,0)",
      "borderDash": [ [0,0], [0,0] ]
    },    
    {
      "data": [98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0],
      "label": "Derek Jones",
      "borderColor": "rgba(14,97,240,1)",
      "backgroundColor": "rgba(255,255,255,0)",
      "borderDash": [ [0,0], [0,0] ]
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true
  }
}
-->
</canvas>
```

# Empty Line Chart {.sub}

<canvas id=“empty_line" data-chart="line">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
    {
      "data": [-52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0],
      "label": "James Smith",
      "borderColor": "rgba(255,105,180,1)",
      "backgroundColor": "rgba(255,255,255,0)",
      "borderDash": [ [0,0], [0,0] ]
    },    
    {
      "data": [98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0],
      "label": "Derek Jones",
      "borderColor": "rgba(14,97,240,1)",
      "backgroundColor": "rgba(255,255,255,0)",
      "borderDash": [ [0,0], [0,0] ]
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true
  }
}
-->
</canvas>

# Bar Chart

```{.json style="font-size:x-small"}
<canvas id=“bar_chart" data-chart="bar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith",
      "backgroundColor": "rgb(0,255,158,1)"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true
  }
}
-->
</canvas>
```

# Bar Chart {.sub}

<canvas id=“bar_chart" data-chart="bar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith",
      "backgroundColor": "rgb(0,255,158,1)"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true
  }
}
-->
</canvas>

# Stacked Bar Chart

```{.json style="font-size:x-small"}
<canvas id=“stacked_bar" data-chart="bar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith",
      "backgroundColor": "rgb(0,255,158,1)"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true,
    "scales": {
            "xAxes": [{
                "stacked": true
            }],
            "yAxes": [{
                "stacked": true
            }]
        }
  }
}
-->
</canvas>
```

# Stacked Bar Chart {.sub}

<canvas id=“stacked_bar" data-chart="bar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith",
      "backgroundColor": "rgb(0,255,158,1)"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones",
      "backgroundColor": "rgba(14,97,240,1)"
    }  ]
  },
  "options": {
    "legend": { "position": "bottom" },
    "responsive": true,
    "scales": {
            "xAxes": [{
                "stacked": true
            }],
            "yAxes": [{
                "stacked": true
            }]
        }
  }
}
-->
</canvas>

# Radar Chart Example

```{.json style="font-size:x-small"}
<canvas id=“radar" data-chart="radar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones"
    }  ]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>
```

# Radar Chart Example {.sub}

<canvas id=“radar" data-chart="radar">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0],
      "label": "James Smith"
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0],
      "label": "Derek Jones"
    }  ]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>

# Doughnut Chart Example

```{.json style="font-size:x-small"}
<canvas id=“doughnut" data-chart="doughnut">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0],
      "label": "James Smith",
      "backgroundColor": ["rgba(255,105,180,1)","rgb(255,127,80,1)","rgb(255,255,102,1)","rgb(0,255,158,1)","rgba(14,97,240,1)","rgba(178,102,255,1)"]
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0],
      "label": "Derek Jones",
      "backgroundColor": ["rgba(255,105,180,1)","rgb(255,127,80,1)","rgb(255,255,102,1)","rgb(0,255,158,1)","rgba(14,97,240,1)","rgba(178,102,255,1)"]
    }  ]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>
```

# Doughnut Chart Example {.sub}

<canvas id=“doughnut" data-chart="doughnut">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May", "June"],
    "datasets":[
        {
      "data": [65.0, 59.0, 80.0, 81.0, 56.0, 55.0],
      "label": "James Smith",
      "backgroundColor": ["rgba(255,105,180,1)","rgb(255,127,80,1)","rgb(255,255,102,1)","rgb(0,255,158,1)","rgba(14,97,240,1)","rgba(178,102,255,1)"]
    },    {
      "data": [28.0, 48.0, 40.0, 19.0, 86.0, 27.0],
      "label": "Derek Jones",
      "backgroundColor": ["rgba(255,105,180,1)","rgb(255,127,80,1)","rgb(255,255,102,1)","rgb(0,255,158,1)","rgba(14,97,240,1)","rgba(178,102,255,1)"]
    }  ]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>

# Pie Chart Example

```{.json style="font-size:x-small"}
<canvas id=“pie" data-chart="pie">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May"],
    "datasets":[
    {
      "data": [25.0, 9.0, 18.0, 20.0, 27.0],
      "label": "James Smith"
    },
    {
      "data": [15.0, 33.0, 8.0, 34.0, 10.0],
      "label": "James Smith"
    }]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>
```

# Pie Chart Example {.sub}

<canvas id=“pie" data-chart="pie">
<!--
{
  "data": {
    "labels":["January", "February", "March", "April", "May"],
    "datasets":[
    {
      "data": [25.0, 9.0, 18.0, 20.0, 27.0],
      "label": "James Smith"
    },
    {
      "data": [15.0, 33.0, 8.0, 34.0, 10.0],
      "label": "James Smith"
    }]
  },
  "options": {
    "legend": {
      "position": "bottom"
  },
    "responsive": true
  }
}
-->
</canvas>