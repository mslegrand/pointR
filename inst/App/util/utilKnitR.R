rmdTemplate<-"---
title: 'svgR in a Markdown Doc'
author: 'author'
date: '5/13/2018'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(svgR)
```

This is an example of using svgR inside an R Markdown. 



**Note** the use of the option results='asis'

```{r, results='asis'}
WH<-c(500,200)
svgR(wh=WH,
ellipse(
cxy=wh/2,
rxy=wh/4,
fill='orange',
stroke='blue',
stroke.width=3
)
)
````

After making changes, simply click the *commit* button to see the results.

"

