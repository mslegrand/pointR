
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  source("util/exGetTag.R", local=TRUE) # some ordinary functions :)
  
# Reactive values----------

  #source("util/serverManagerSrc.R", local=TRUE)
  
  request<-reactiveValues(
    code=NULL,
    sender='startup',
    refresh=NULL
  )
  
  triggerRefresh<-function(sender, n=1){
    request$sender=sender
    request$refresh= runif(1, n, n+1)
  }
  
  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  
  panels<-reactiveValues(right="Points")
  rightPanel<-reactive({panels$right})
  
  updateRightPanel<-function(panel){ panels$right<-panel}

  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
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
  
  getCode<-reactive({
    request$code
  })
  
  # setCode<-function(txt, what="history"){
  #   #srcPushTxt(txt,what)
  # }
  
  # setSrcCode<-function(txt, what="history"){
  #   srcPushTxt(txt,what)
  # }
  # 
  # 
  
  getPtName<-reactive({selectedPoint$name})
  getPtIndex<-reactive({selectedPoint$point.index})
  #-----------------------
 
  
  getErrorMssg<-reactive({ mssg$error })
  getPtDefs<- reactive({ 
    ex.getPtDefs(getCode() ) 
  })  #extract points from user code
  
  #gets the tagged names
  getTagNameChoices<-reactive({
    cat("Inside getTagNameChoices:: getPtDefs$df=", str(getPtDefs()$df),"\n")
    intersect(names(getPtDefs()$pts), names(getPtDefs()$df))
  })
  
  getSelectInfo<-reactive({ #used by pointsBar only??
    name<-getPtName()
    indx<-getPtIndex()
    pts<-getPtDefs()$pts
    ex.getSelectInfo(pts, name, indx)
  })
  
  getPts<-reactive({
    ptdef<-getPtDefs()
    ptdef[[getPtName()]]
  })
  
  #gets a tagged name (=ptName unless ptName is not tagged)
  getTagName<-reactive({
    
    cat("getPtName()=",getPtName(),"\n")
    exGetTagName( getTagNameChoices(), getPtName() )
  })
  getTagIndexChoices<-reactive({
    cat("getTagName()=",getTagName(),"\n")
    cat(str(getPtDefs()))
    getPtDefs()$df[[getTagName()]]$tag
  })
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
    length(getCode()) >0 &&
    nchar(getCode())>0 &&
    ( 
      grepl("class\\s*=\\s*'draggable'",getCode() ) || 
      grepl('class\\s*=\\s*"draggable"',getCode() )
    )
  }) 

# Event Observers-------------------------------- 
  observe({input$messageFromAce
    isolate({
      cat("observe input$messageFromAce")
      if(
        length(input$messageFromAce$code)>0 &&
        length(input$messageFromAce$sender)>0
      ){
        request$code<-input$messageFromAce$code
        request$sender<-input$messageFromAce$sender
        if(length(input$messageFromAce$dirty)>0){
          editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
        }
        
        cat("request$sender=",request$sender,"\n")
        cat('dirty= ', input$messageFromAce$dirty )
        processCommit()
        # if(request$sender=='cmd.commit'){
        #   processCommit()
        # } e
      }
    })
  })
  
  updateAceExtDef<-function(newPtDef, sender ){
    
    replacementList<-ptDef2ReplacementList(newPtDef, getCode() )
    #src<-df2Source(src,dfList) #insert points into src
    
    if( length(replacementList)>0 ){
      session$sendCustomMessage(
        type = "shinyAceExt",
        list(id= "source", replacement=replacementList, sender='tag.pt.button', ok=1)
      )
    }
  }
  
  observe({
    request$sender
    isolate({
      if(request$sender=='startup'){
        cat("observe:request$sender")
        cmdFileNew()
      }
        
    })
  })
  
  observe({
    request$sender
    isolate({
      if(request$sender=='startup')
        cmdFileNew()
    })
  })
  
  # observe({request$refresh
  #         isolate({
  #           if(length(request$code==0){
  #             session$sendCustomMessage(
  #               type = "shinyAceExt",
  #               list(id= "source",  getValue= TRUE)
  #             )
  #           }
  #         })
  # })
  # 
  
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

#------------------rightPanel--------------------------------

source("rightPanel/serverPlotBarPoints.R", local=TRUE) 
source("rightPanel/serverPlotBarTagValues.R", local=TRUE)  
source("rightPanel/serverPlotBarTagDrag.R", local=TRUE)  
source("rightPanel/serverPlotBarTransform.R", local=TRUE) 

source("rightPanel/serverLog.R", local=TRUE) 
source("rightPanel/serverPlotBar.R", local=TRUE)
source("rightPanel/serverOptions.R", local=TRUE) 
#-----MOUSE CLICKS---------------------------------
source("rightPanel/serverMouseClicks.R", local=TRUE)
source("rightPanel/serverPlotBar.R", local=TRUE)  
  
  
#---------------leftPanel--------------------------
#------buttons
source("leftPanel/serverButtons.R",local = TRUE)
#------menu
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
  

  


 
})
