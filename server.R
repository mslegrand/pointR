
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)
library(svDialogs) #!!!todo: replace this

#options(shiny.error = recover)

#----begin external rc------------
source("utilFormat.R")
source("utilParser.R")
source("utilptR.R")
source("utilTransform.R")

defTag<-"ptR"

js.scripts<-list(
  Points=readFile("www/pointsIO.js"),
  Translate=readFile("www/transIO.js"),
  Rotate=readFile("www/rotIO.js"),
  Scale=readFile("www/scaleIO.js"),
  transTag=readFile("www/transTag.js")
)

#---begin external---------
#these functions should be ultimately place in another file
getSVGWH<-function(){ c(650,620)}

pts2Source<-function(txt,ptRList){
  replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
  txt<-replaceDef(txt, replacement, defTag=defTag) 
}

df2Source<-function(txt, dfList){
  if(length(dfList)>0){
    replacement<-formatDFDefs(dfList)
  } else {
    replacement<-""
  }
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
  if(length(choices)==0 ){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  
  if(length(selected)<1 || !(selected %in% choices) ){ # a new choice
    #pick the first choice candidate
    selected=choices[1]
    pts<-ptRList[[selected]]
    point.index<-length(pts)/2
    rtv<-list(
      selected=selected,
      point.index=point.index
    )
    return(rtv)
  }
  #default: an existing choice
  point.index<-min(point.index, length( ptRList[[selected]])/2 ) #cannot be longer than the number of points
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


  #ordinary fns
  exGetTagName<-function(tagNameChoices, ptChosen){
    if(length(tagNameChoices)>0){
      if(length(ptChosen)>0 && (ptChosen %in% tagNameChoices)){
        tagChosen<-ptChosen
      } else {
        tagChosen<-tail(tagNameChoices,1)
      }
    } else {
      tagChosen<-NULL
    }
    tagChosen
  }
  
  # tagIndxCƒhoices<-getTagIndexChoices()
  # point.index<-selected$point.indx
  exGetTagIndx<-function(tagIndxChoices, point.indx){
    if(length(tagIndxChoices)<1 || length(point.indx)<1){
     return(NULL)
    }
    #point.indx<-as.numeric(point.indx)
    if( point.indx>0 ){
        t.point.indx<-max(tagIndxChoices[ tagIndxChoices<= point.indx] )
    } else {
        0
    }
  }  
  
# Reactive values----------
  #Eventually we want a stack of code changes so we can do an undo
  user <- reactiveValues( code=codeTemplate) #  internal copy of user code
  backup<-reactiveValues( code=codeTemplate) # last good copy of user code
  file <-reactiveValues( name="newFile.R")       #  file path
  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  #tagVal<-reactiveValues(hasTag=FALSE)
  reactiveTag<-reactiveValues(freq=list())
  displayOptions<-reactiveValues(
    insertMode=TRUE,
    showGrid=TRUE,
    ptMode="Normal"
  )
  
  #---
  getCode<-reactive({ user$code })
  getCodeBackup<-reactive({ backup$code })
  
  getPtName<-reactive({selectedPoint$name})
  getPtIndex<-reactive({selectedPoint$point.index})
    
  #-----------------------
  barName<-reactive({input$plotNavBar})
  mssg<-reactiveValues(error="") 
  
# Reactive expressions------------- 
  getErrorMssg<-reactive({ mssg$error })
  getPtDefs<-        reactive({ ex.getPtDefs(user$code) })  #extract points from user code
  getTagNameChoices<-reactive({
  intersect(names(getPtDefs()$pts), names(getPtDefs()$df))
  })
  getSelectInfo<-reactive({ #used by pointsBar only??
    name<-getPtName()
    indx<-getPtIndex()
    pts<-getPtDefs()$pts
    ex.getSelectInfo(pts, name, indx)
    #ex.getSelectInfo(getPtDefs()$pts, getPtName(), getPtIndex())
  })
  getTagName<-reactive({exGetTagName( getTagNameChoices(), getPtName() )})
  getTagIndexChoices<-reactive({getPtDefs()$df[[getTagName()]]$tag})
  getTagIndex<-reactive({ 
    choices<-getTagIndexChoices()
    indx<-getPtIndex()
    exGetTagIndx(choices, indx )
  })
  getTagColChoices<-reactive({
    df<-getPtDefs()$df[[getTagName()]]
    tagColChoices<-setdiff(names(df),"tag")
    tagColChoices
  })
  getTagCol<-reactive({ 
    if(length(getTagColChoices()==0)){
      rtv<-NULL
    }else{
      if(length(tagValInfoList)>0 && 
        !(is.null(tagValInfoList$colName )) &&
        length(tagValInfoList$colName())>0){
        if(tagValInfoList$colName() %in% getTagColChoices()){
          rtv<-tagValInfoList$colName()
        } else {
          rtv<-getTagColChoices()[1]
        }
      }
      rtv<-NULL
    }
    rtv
  })
  getTagValueChoices<-reactive({
    df<-getPtDefs()$df[[getTagName()]]
    tagColChoice<-getTagCol()
    if(!is.null(tagColChoice)){
        tagValueChoices<-df[[tagColChoice]]
      } else {
        tagValueChoices<-NULL
      }
      tagValueChoices
  }) 
  getTagValue<-reactive({
    tagIndx<-getTagIndex()
    tagVals<-getTagValueChoices()
    if(length(tagVals)>0 && length(tagIndx)>0 ){
        tagValue<-subset(df,df$tag==tagIndx)[[getTagCol()]]
      } else {
        tagValue<-NULL
      }
      tagValue
  })
  
  #call with ptsDefs$df as 


  
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
 
  








#--------------------------------------------------
# !!!TO DO: ADD HANDLER FOR POINT AND INDEX


source("serverPlotBarPoints.R", local=TRUE) 
# --------------input$plotNavBar=="Tags"----------------  
source("serverPlotBarTagValues.R", local=TRUE)  
source("serverPlotBarTagDrag.R", local=TRUE)  
  
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
