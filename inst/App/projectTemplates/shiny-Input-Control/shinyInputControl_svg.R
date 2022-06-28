library(svgR)
library(tidyverse)

#-------- params ----------------------
#` default params
WH<-c(400,400)
CMDS<-"alert('control triggered')"
ID<-'mycontrol'

# STEP 1.1: if needed, add to params, for example
# value=200

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
     #STEP 4: mouse events (such as onclick=CMDS)
)






