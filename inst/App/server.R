
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
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }

  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0          #  selected pt.indx (column) in current point array
  )
  
  # selectedPoint <- reactiveValues(
  #   tibble.name="x", #NULL,       # name of current point array
  #   point.index=0,          #  selected pt.indx (column) in current point array
  #   point.col.name='pts',
  #   row.num=1
  # )
  
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
    showGrid=FALSE,
    ptMode="Normal"
  )
  
  mssg<-reactiveValues(error="") 
  
  
  shinyFileChoose(input, "buttonFileOpenHidden", session=session, roots=c(wd="~") ) #hidden
  shinyFileChoose(input, "buttonSnippetOpen", session=session, roots=c(wd="~"),  filetypes=c('', 'snp') ) #hidden
  shinyFileSave(input, "buttonFileSaveHidden", session=session, roots=c(wd="~") ) #hidden
  
  
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
    exGetTagName( getTagNameChoices(), getPtName() )
  })
  getTagIndexChoices<-reactive({
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
      if(
        length(input$messageFromAce$code)>0 &&
        length(input$messageFromAce$sender)>0
      ){
        #cat('observe input$messageFromAce:: entering\n')
        request$code<-input$messageFromAce$code
        request$sender<-input$messageFromAce$sender
        #cat('sender=',request$sender,"\n")
        #cat('code=',nchar(request$code),"\n")
        if(length(input$messageFromAce$dirty)>0){
          editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
        }
        #processCommit()
        if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow' )){
           processCommit()
        } 
        if( request$sender %in% 'cmd.openFileNow'){
          #set point.index to end of points (if points)
        }
        if(request$sender %in% 'cmd.saveFileNow'){
          #cat('observe {input$messageFromAce:: cmd.saveFileNow\n')
          datapath<-input$messageFromAce$auxValue
          txt<-input$messageFromAce$code
          writeLines(txt, datapath)
          setCurrentFilePath(datapath)
          editOption$currentFile<-basename(datapath)
          editOption$currentDirectory<-dirname(datapath)
          session$sendCustomMessage(
            type = "shinyAceExt",
            list(id= "source", setClean=TRUE, sender='cleanPlease')
          )
          
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
        # displayOptions$insertMode=TRUE
        # displayOptions$showGrid=FALSE
        # displayOptions$ptMode="Normal"
        
        cmdFileNew()
      }
      if(request$sender %in% c( "cmd.openFileNow", "cmd.newFile")){ #!!! check these names
        # get valid point name, then set index to last valid index. (length of points?)
        pd<-getPtDefs()
        if(length(pd)>0){
          pts<-pd$pts
          name<-tail(names(pts),1)
          l<-length(pts[[name]])
          selectedPoint$name<-name
          selectedPoint$point.index<-l/2
        }
      } 
        
    })
  })
  
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
