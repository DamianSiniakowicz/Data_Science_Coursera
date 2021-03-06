---
title       : Iris Classification
subtitle    : using random forests
author      : damian
job         : messing around on the internet
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Iris Data

The Iris dataset contains observations about 5 characteristics of 150 irises.


```r
data("iris")
dim(iris)
```

```
## [1] 150   5
```

We want to predict the Species characteristic.


```r
names(iris)
```

```
## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
## [5] "Species"
```

```r
class(iris$Species)
```

```
## [1] "factor"
```

--- .class #id 

## Random Forest

Random Forest is a machine learning algorithm

1. generate decision trees
2. assign to each leaf the most common class of training examples which map to that leaf.
3. pass a oos observation into each tree, and assign it the mode class of the leaves it maps to.

---

## User Interface

The user has to set the values of two variables
* the number of trees they want the random forest algorithm to build
* the number of out of sample error measurements they want to go into the estimate of out of sample error

---

## Conclusion

* We hope you like our app!


