---
center: False
css: 'thebe-styling.css'
header-includes: |
    <script type="text/x-thebe-config">
        {
        bootstrap: true,
        selector: "[data-executable=true]",
        binderOptions: {
         repo: "binder-examples/requirements"
        },
        kernelOptions: {
          name: "python3",
        },
      }
    </script>
    <script src="https://unpkg.com/thebelab@0.4.0/lib/index.js"></script>
height: 500.0
history: True
margin: '0.0'
maxScale: 1.0
minScale: 1.0
subtitle: Python Kernel
title: ThebeLab Test
width: 960.0
---
---

# ThebeLab Binder Deck

## ThebeLab

A Javascript client library for Jupyter servers that uses the Jupyter API

-   [minrk/thebelab](https://github.com/minrk/thebelab)
-   [ThebeLab - ThebeLab  documentation](https://thebelab.readthedocs.io/en/latest/)

## Binder

ThebeLab uses Binder to run the Jupyter server with the IHaskell kernel from a
custom Docker image on GitHub.

-   [The Binder Project](https://mybinder.org)
-   [gibiansky/IHaskell](https://github.com/gibiansky/IHaskell)
-   [monofon/plc-notebooks](https://github.com/monofon/plc-notebooks)

# A ThebeLab Code Block

``` {.python data-executable="true" data-language="python"}
print "Hallo!"
```

# A ThebeLab Code Block

``` {.python data-executable="true" data-language="python" style="max-height:400px"}
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
x = np.linspace(0,10)
plt.plot(x, np.sin(x))
plt.plot(x, np.cos(x))
```
