
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  source("util/exGetTag.R", local=TRUE) # some ordinary functions :)
  
# Reactive values----------

  #source("util/serverManagerSrc.R", local=TRUE) no longer used
  
  request<-reactiveValues(
    code=NULL,
    sender='startup',
    refresh=NULL # to be used to force a code refresh???
  )
  
  triggerRefresh<-function(sender, rollBack=TRUE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack)
    )
  }
  
  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0          #  selected pt.indx (column) in current point array
  ) 
  
  panels<-reactiveValues(
    left='source',   #to be used as editor name later, for connecting to right graphics
    right="Points"
  )
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
  showGrid<-reactive({displayOptions$showGrid})
  
  #--- yes unless tagged with freq or no points to tag 
  isTaggable<-reactive({ 
    name<-getPtName()
    !is.null(name) && getPtIndex()>0 &&  is.null(reactiveTag$freq[[name]])
  })
  
  getCode<-reactive({request$code})
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
    
    #cat("getPtName()=",getPtName(),"\n")
    exGetTagName( getTagNameChoices(), getPtName() )
  })
  getTagIndexChoices<-reactive({
    #cat("getTagName()=",getTagName(),"\n")
    #cat(str(getPtDefs()))
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
      cat("\nobserve input$messageFromAce\n")
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
        cat('dirty= ', input$messageFromAce$dirty,"\n" )
        #processCommit()
         if(request$sender=='cmd.commit'){
           processCommit()
         } 
      }
    })
  })
  
  updateAceExtDef<-function(newPtDef, sender ){
    #src<-df2Source(src,dfList) #insert points into src
    replacementList<-ptDef2ReplacementList(newPtDef,getCode() )
    if( length(replacementList)>0 ){
      session$sendCustomMessage(
        type = "shinyAceExt",
        list(id= "source", replacement=replacementList, sender=sender, ok=1)
      )
    }
  }
  
  updateAceExt<-function(sender, ...){
    data<-as.list(...)
    if(length(data)>0){
      data<-c(list(id=='source', sender=sender), data )
      session$sendCustomMessage(
        type = "shinyAceExt",
        data
      )
    }
  }
  
  observe({
    request$sender
    isolate({
      if(request$sender=='startup'){
        #cat("observe:request$sender")
        cmdFileNew()
      }
        
    })
  })
  
  # observe({
  #   request$sender
  #   isolate({
  #     if(request$sender=='startup')
  #       cmdFileNew()
  #   })
  # })
  
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
