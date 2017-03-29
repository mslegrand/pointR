
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  source("./util/exGetTag.R", local=TRUE) # some ordinary functions :)
  
# Reactive values----------
  #Eventually we want a stack of code changes so we can do an undo
  user <- reactiveValues( code=codeTemplate) # internal copy of user code
  backup<-reactiveValues( code=codeTemplate) # last good copy of user code
  
  #file <-reactiveValues( name="newFile.R")       #  file path
  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  panels<-reactiveValues(right="Points")
  rightPanel<-reactive({panels$right})
  updateRightPanel<-function(panel){ panels$right<-panel}

  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  #tagVal<-reactiveValues(hasTag=FALSE)
  
  reactiveTag<-reactiveValues(freq=list())
  displayOptions<-reactiveValues(
    insertMode=TRUE,
    showGrid=TRUE,
    ptMode="Normal"
  )
  mssg<-reactiveValues(error="") 
  
  
  # Reactive expressions------------- 
   #---
  isTaggable<-reactive({ 
    name<-getPtName()
    !is.null(name) && getPtIndex()>0 &&  is.null(reactiveTag$freq[[name]])
  })
  
  getCode<-reactive({ user$code })
  getCodeBackup<-reactive({ backup$code })
  
  getPtName<-reactive({selectedPoint$name})
  getPtIndex<-reactive({selectedPoint$point.index})
  #-----------------------
 
  
  getErrorMssg<-reactive({ mssg$error })
  getPtDefs<- reactive({ 
    ex.getPtDefs(user$code) 
  })  #extract points from user code
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
      ptTags<-NULL
    }
    tagRList<-getPtDefs()$df 
    if(!is.null(tagRList)){
      ptTags<-tagRList[[ptName]]
    } else {
      ptTags<-NULL
    }
    ptTags
  })
  
  usingTransformDraggable<-reactive({ 
    grepl("class='draggable'",user$code) ||
    grepl('class="draggable"',user$code)
  }) 

# Event Observers-------------------------------- 
  
#help
  source("leftPanel/helpSVG.R", local=TRUE) 
  
#---navbar disable /enabler controls
  observe({
    tagsMissing<-is.null(getPtDefs()$df)
    isolate({
      if(tagsMissing){
        disableDMDM(session, "plotNavBar", "Tags")
      } else {
        enableDMDM(session, "plotNavBar", "Tags")
      }
    })
  })
  
  
  
  observe({
    using<-usingTransformDraggable()
    isolate({
      if(using){
        enableDMDM(session, "plotNavBar", "Transforms")
      } else {
        disableDMDM(session, "plotNavBar", "Transforms")
      }
    })
    
  }
  )
  
  

#--------------------------------------------------

source("rightPanel/serverPlotBarPoints.R", local=TRUE) 
# --------------input$plotNavBar=="Tags"----------------  
source("rightPanel/serverPlotBarTagValues.R", local=TRUE)  
source("rightPanel/serverPlotBarTagDrag.R", local=TRUE)  
source("rightPanel/serverPlotBarTransform.R", local=TRUE) 
   
  
source("rightPanel/serverLog.R", local=TRUE) 
source("rightPanel/serverPlotBar.R", local=TRUE)
source("rightPanel/serverOptions.R", local=TRUE)  
#---------------Button handlers--------------------------
source("leftPanel/serverButtons.R",local = TRUE)

  
#--------------------------------navbarMenuBar--------

source("leftPanel/cmdFileSaveAs.R", local=TRUE)  
source("leftPanel/cmdFileSave.R", local=TRUE)  
source("leftPanel/cmdFileNew.R", local=TRUE)  
source("leftPanel/cmdFileOpen.R", local=TRUE)  
source("leftPanel/cmdFileQuit.R", local=TRUE)  
source("leftPanel/cmdFileExportSvg.R", local=TRUE) 
source("leftPanel/cmdOptionsTheme.R",local=TRUE)
source("leftPanel/cmdOptionsFontSize.R", local=TRUE)  
source("leftPanel/cmdFileSnippet.R",local=TRUE)
source("leftPanel/cmdAbout.R",local=TRUE)
source("leftPanel/serverEditBar.R",local=TRUE)
  
  
  
#-----------------------MOUSE CLICKS---------------------------------
source("rightPanel/serverMouseClicks.R", local=TRUE)
source("rightPanel/serverPlotBar.R", local=TRUE)  

  


 
})
