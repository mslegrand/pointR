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

ioslidesTemplate<-"---
title: 'svgR in an ioslides presentation'
author: 'author'
date: '5/13/2018'
output: slidy_presentation
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(svgR)
```


This is an example of using svgR inside an R Markdown. 

# Slide One


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


# slide two

```{r, results='asis'}
WH<-c(500,200)
svgR(wh=WH,
  rect(
    cxy=WH/2,
    wh=WH/4,
    fill='blue',
    stroke='orange',
    stroke.width=10
  )
)
````
"



