---
eqLabels: roman
---

# 

more information see

[http://lierdakil.github.io/pandoc-crossref/](http://lierdakil.github.io/pandoc-crossref/)

# ref before label

This is a reference to equation [@eq:eq1]

# Equation

$$ math = formula + 2
2+2= 4 
$$ {#eq:eq1}

# Equation Reference

This is a reference to equation [@eq:eq1; @eq:eq2]

# Another Equation

$$ math = formula + 2
2+2= 4 
$$ {#eq:eq2}

# Figure

![a](include/06-metal.png){#fig:figureRefA}

![b](include/06-metal.png){#fig:figureRefB}

# test

Reference to [@Fig:figureRefA] and a ref to [@fig:figureRefB]



# code

```{#lst:code .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```

# Code reference

This is a reference to listing [@lst:code]

# Table

a  | b |  c
--- |--- |---
1  | 2|   3
4  | 5  | 6

: Caption {#tbl:label}

# Table Reference

This is a reference to table [@tbl:label]