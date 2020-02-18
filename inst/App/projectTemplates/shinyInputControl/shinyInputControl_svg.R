library(svgR)
library(tidyverse)

#-------- params ----------------------
#` default params
WH<-c(400,400)
CMDS<-"alert('control triggered')"
ID<-'mycontrol'

# STEP 1.1: add params

#----------function override of params---------- 
if(exists("params") ){
    for(n in names(params)){
        assign(n, params[[n]])
    }
}

#-----any R helper code goes here--------------------------

svgR(wh=WH,
     #your custom code goes here
     NULL
     #STEP 1.2: add to svgR to create image
     #STEP 2: mouse events (such as onclick=CMDS)
)






