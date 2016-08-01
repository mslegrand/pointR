
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
  
  #---just added this group
  #getCode<-reactive({ user$code })
  
  getPtName<-reactive({selectedPoint$name})
  getPtIndex<-reactive({selectedPoint$point.index})
    
  #-----------------------
  barName<-reactive({input$plotNavBar})
  mssg<-reactiveValues(error="") 
  
# Reactive expressions------------- 
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
 
  
# --------------input$plotNavBar=="Points"---------------- 
output$PointsPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='Points'", modulePointsBarUI("pointsBar"))
})

pointInfoList<-callModule( 
  module=modulePointsBar, 
  id="pointsBar", 
  barName=reactive(input$plotNavBar),
  getSelectInfo=getSelectInfo, #not the best way, but ...
  getPtDefs=getPtDefs, 
  name=getPtName, 
  index=getPtIndex
)

observe({
  name<-pointInfoList$name()
#  isolate({
    if(!is.null(name)){
      selectedPoint$name<-pointInfoList$name()
    }
#  })
})

observe({
  value<-pointInfoList$tagFreq()
  isolate({
    name<-getPtName()
    if(is.null(getPtDefs()$pts) || is.null( name )) { return() }
    ptNames<-names(getPtDefs()$pts)
    freq<-reactiveTag$freq
    freq<-lapply( ptNames, function(n)freq[[n]] )
    if( !is.null(value) && value=="Off"){
      value<-NULL
    } else {
      selection<-getPtName()
      tagList<-getPtDefs()$df
      if(!is.null(tagList) && !is.null(tagList[[selection]])){
        #get the last tagged element and iterate the tagging
        dn<-as.integer(value)
        df<-tagList[[selection]]
        df1<-tail(df,1)
        n1<-df1$tag
        ptList<-getPtDefs()$pts
        n2<-length(ptList[[selection]])/2
        if(n2>n1){
          s<-seq(from=n1,to=n2,by=dn)
          s<-s[-1]
          if(length(s)>0){
            df2List<-lapply(s, function(tn){ df2<-df1; df2$tag<-tn; df2})
            df3<-do.call(rbind, df2List )
            df4<-rbind(df,df3)
            tagList[[selection]]<-df4
            src<-user$code
            src<-df2Source(src,dfList = tagList)
            user$code<-src 
          }
        }
      }
    }
    freq[[name]]<-value
    reactiveTag$freq<-freq
  })  
})


showGrid<-reactive({pointInfoList$showGrid()})
displayMode<-reactive({pointInfoList$displayMode()})
# --------------input$plotNavBar=="tagValues"---------------- 
 output$TagValuesPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='tagValues'", moduleTagValUI("tagValBar"))
 })

tagValInfoList<-callModule(
  module=moduleTagVal,
  id="tagValBar",
  barName=reactive(input$plotNavBar),
  getTagNameChoices=getTagNameChoices,
  getTagName=getTagName,
  getTagIndexChoices=getTagIndexChoices,
  getTagIndex=getTagIndex,
  getTagColChoices=getTagColChoices,
  getTagCol=getTagCol,
  getTagValueChoices=getTagValueChoices,
  getTagValue=getTagValue
)

observe({
  name<-tagValInfoList$name()
  index<-tagValInfoList$index()
#  isolate({
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
#  })
})




# --------------input$plotNavBar=="dragTag"---------------- 

 output$TagDragPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='dragTag'", moduleTagDragUI("tagDragBar"))
 })

tagDragInfoList<-callModule(
  module=moduleTagDrag,
  id="tagDragBar",
  barName=reactive(input$plotNavBar),
  getTagNameChoices=getTagNameChoices,
  getTagName=getTagName,
  getTagIndexChoices=getTagIndexChoices,
  getTagIndex=getTagIndex
)

observe({
  name<-tagDragInfoList$name()
  index<-tagDragInfoList$index()
#  isolate({
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
#  })
})


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
