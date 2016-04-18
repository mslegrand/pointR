
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)
library(svDialogs)


#options(shiny.error = recover)

#----begin external rc------------
source("utilFormat.R")
source("utilParser.R")
source("ptrUtil.R")
source("trUtils.R")

defTag<-"ptR"

js.scripts<-list(
  Points=readFile("www/pointsIO.js"),
  Translate=readFile("www/transIO.js"),
  Rotate=readFile("www/rotIO.js"),
  Scale=readFile("www/scaleIO.js")
)


pts2Source<-function(txt,ptRList){
  replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
  txt<-replaceDef(txt, replacement, defTag=defTag) 
}

df2Source<-function(txt, dfList){
  replacement<-formatDFDefs(dfList)
  txt<-replaceDef(txt, replacement, defTag="tagR") 
}

#used by open and commit
preProcCode<-function(src){
  ptDefs<-ex.getPtDefs(src)
  ptRList<-ptDefs$pts
  dfList<-ptDefs$df
  src<-pts2Source(src,ptRList)
  if(!is.null(dfList)){
    src<-df2Source(src, dfList)
  }
  return(src)
}


# called by either a new/load source or upon a commit
ex.getSelectInfo<-function(ptRList, selected, point.index){
  choices<-names(ptRList)
  if(length(choices)==0){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  if(!(selected %in% choices)){ # a new choice
    #pick the first choice candidate
    N<-1
    rtv<-list(
      selected=choices[N],
      point.index=length(ptRList[[N]])/2
    )
    return(rtv)
  }
  #default: an existing choice
  point.index<-min(point.index, length( ptRList[[selected]])/2 )
  rtv<-list(
    selected=selected, 
    point.index=point.index
  )
  return(rtv)  
}

#
#----end external ------------

#---begin server--------------

shinyServer(function(input, output,session) {

# Reactive values----------
  user <- reactiveValues( code=codeTemplate) #  internal copy of user code
  backup<-reactiveValues( code=codeTemplate) # last good copy of user code
  file <-reactiveValues( name="newFile.R")       #  file path
  selectedPoint <- reactiveValues(
    name=NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  tagVal<-reactiveValues(hasTag=FALSE)
  
  reactiveTag<-reactiveValues(freq=list())
  
  #errorText <-reactiveValues( mssg="hello") 
  
  init<-reactiveValues(val=0)                   #  kludge for initialization (shouldn't need this)
  mssg<-reactiveValues(error="")
  
# Reactive expressions------------- 
  getPtDefs<-reactive({ ex.getPtDefs(user$code) })  #extract points from user code

  
# If the user adds a point, use reactivTag$freq  
# observe(addingPt,
#   isolate({
#     if(pointName %in% namesreactiveTag$freq){
#       indxLast<-getIndx of last tag with pointName
#       indxPt<-getIndx of Pt with pointName
#       if(indxPt==(indxLast + reactiveTag$freq[[pointName]]){
#           tag this point
#       }
#     }
#   })
# )
  
  

# Event Observers--------------------------------  

# -----------ACE EDITOR------------------------
observeEvent(
  user$code, {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=user$code)
    }
  }
)
 
source("serverPlotBarPoints.R", local=TRUE) 
# --------------input$plotNavBar=="Tags"----------------  
source("serverPlotBarTags.R", local=TRUE)  
  
#---------------Button handlers--------------------------
source("serverButtons.R",local = TRUE)

  

#--------------------------------navbarMenuBar--------
source("serverEditBar.R",local=TRUE)
  
#-----------------------MOUSE CLICKS---------------------------------
source("serverMouseClicks.R", local=TRUE)
  
#---------BEGIN OUTPUT PANELS------------------------------------
#------fileName-------------
  output$fileName <- renderText({ 
    fileName<-file$name
    if(is.null(fileName) ){
      fileName==""
    }
    paste("Editing", basename(fileName))
  })
  
#------Graphical output-------------------  
source("serverSVGHTML.R", local=TRUE)
  
#-----log panel---------------------------
  output$out_log <- renderText({
    mssg$error
  })

#---------END OUTPUT PANELS------------------------------------

 
})
