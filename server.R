
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  js$disableMenu('#plotNavBar li:nth-child(2)')
  js$disableTab("Transforms")
  
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
    if(length(tagIndxChoices)<1 ){
     return(NULL)
    }
    if(  length(point.indx)<1){
     point.index<-max(tagIndxChoices)
    } else {
     #point.indx<-as.numeric(point.indx)
      if( point.indx>0 ){
          t.point.indx<-max(tagIndxChoices[ tagIndxChoices<= point.indx] )
      } else {
          0
      }    
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
  
  getPts<-reactive({
    ptdef<-getPtDefs()
    ptdef[[getPtName()]]
  })
  
  getTagName<-reactive({exGetTagName( getTagNameChoices(), getPtName() )})
  getTagIndexChoices<-reactive({getPtDefs()$df[[getTagName()]]$tag})
  getTagIndex<-reactive({ 
    choices<-getTagIndexChoices()
    indx<-getPtIndex()
    exGetTagIndx(choices, indx )
  })
#  getTagColChoices<-reactive({
#  print("getTagColChoices")
#    df<-getPtDefs()$df[[getTagName()]]
#    print(df)
#    tagColChoices<-setdiff(names(df),"tag")
#    print(tagColChoices)
#    tagColChoices
#  })
#  getTagCol<-reactive({ 
#  print("getTqagCol")
#  print(getTagColChoices())
#    if(length(getTagColChoices())==0){
#      rtv<-NULL
#    }else{
#      if(
#        length(tagValInfoList)>0            && 
#        !(is.null(tagValInfoList$colName )) &&
#        length(tagValInfoList$colName())>0){
#        if(tagValInfoList$colName() %in% getTagColChoices()){
#          rtv<-tagValInfoList$colName()
#        } else {
#          rtv<-getTagColChoices()[1]
#        }
#      } else{
#        rtv<-NULL
#      }
#    }
#    rtv
#  })
#  getTagValueChoices<-reactive({
#    df<-getPtDefs()$df[[getTagName()]]
#    tagColChoice<-getTagCol()
#print("Inside getTagValueChoices")
#print(tagColChoice)
#print( df[[tagColChoice]] )
#    if(!is.null(tagColChoice)){
#        tagValueChoices<-df[[tagColChoice]]
#      } else {
#        tagValueChoices<-NULL
#      }
#      tagValueChoices
#  }) 
#  getTagValue<-reactive({
#print("Inside getTagValue")
#    tagIndx<-getTagIndex()
#    tagVals<-getTagValueChoices()
#print(tagIndx)
#print(tagVals)
#    if(length(tagVals)>0 && length(tagIndx)>0 ){
#        df<-getPtDefs()$df[[getTagName()]]
#        tagValue<-subset(df,df$tag==tagIndx)[[getTagCol()]]
#      } else {
#        tagValue<-NULL
#      }
#      tagValue
#  })
  
  #this is tagDisplay Mode
  getDisplayModeTag<-reactive({
    if(displayMode()=="hidden"){
    } else {
      displayMode()
    }
  })
  
  getTagDF<-reactive({
    ptName<-getPtName()
    if(is.null(  getPtName() )){
      ptTags<NULL
    }
    tagRList<-getPtDefs()$df 
    if(!is.null(tagRList)){
      ptTags<-tagRList[[ptName]]
    } else {
      ptTags<-NULL
    }
    ptTags
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
  
  usingTransformDraggable<-reactive({ 
    grepl("class='draggable'",user$code) ||
    grepl('class="draggable"',user$code)
  }) 

# Event Observers--------------------------------  

  observe({
    tagsMissing<-is.null(getPtDefs()$df)
    isolate({
      name<-'#plotNavBar li:nth-child(2)'
      if(tagsMissing){
        js$disableMenu(name)
      } else {
        js$enableMenu(name)
      }
    })  
  })
  
  
  
  observe({
        using<-usingTransformDraggable()
    if(using){
      js$enableTab("Transforms")
      
    } else {
      js$disableTab("Transforms")
    }
  }
  )
  
  
  
  
  
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
source("serverPlotBarTransform.R", local=TRUE)  

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
#source("serverSVGHTML.R", local=TRUE)
  
#-----log panel---------------------------
  output$out_log <- renderText({
    mssg$error
  })

#---------END OUTPUT PANELS------------------------------------

 
})
