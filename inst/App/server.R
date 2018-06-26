
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
source("util/exGetTag.R",  local=TRUE) # some ordinary functions :)

# Reactive values----------
  request<-reactiveValues(
    code=NULL, 
    mode='ptr',
    sender='startup',
    tabs=NULL
  )
  
  drippetSelection<-reactiveValues(
    current=NULL,
    all=list()
  )
  
  addDrippets<-function(dnName, dnippets, select=dnName ){
    drippetSelection$all[[dnName]]=dnippets
    drippetSelection$current<-c(drippetSelection$current, select)
  }
  
  observeEvent(c( drippetSelection$all),{
    updateAwesomeCheckboxGroup(session, inputId="selectedDDDnippets", choices = names(drippetSelection$all),
                              selected = drippetSelection$current, inline = FALSE, status = "primary")
    
    
  })
  
  observeEvent(input$selectedDDDnippets,{
    selected=input$selectedDDDnippets
    #browser()
    cat('\nselected=',format(selected),"\n\n")
    drippetSelection$selected=selected
    dnippets<-drippetSelection$all[selected]
    cat('+++++++++++dnippets are\n')
    print(dnippets)
    dnippets<-unlist(dnippets,recursive=F)
    names(dnippets)<-NULL
    cat('+++++++++++dnippets are\n')
    print(dnippets)
    if(length(dnippets)==0){
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
    } else{
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
    }
    
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  
  setTabRequest<-function(sender, tabs){
    request$sender<-sender
    request$tabs<-tabs
  }
  getSender<-reactive({request$sender})
  peekTab<-reactive( {request$tabs[1]} )
  popTab<-reactive({
    tab<-request$tabs[1]
    request$tabs<-request$tabs[-1]
    tab
  })
  
  
  getMode<-reactive({
    request$mode
  })
  
  getCode<-reactive({
      request$code
  })
  
  
  mssg<-reactiveValues(
    error="",
    capturedOutput=""
  ) 
  setErrorMssg<-function(errMssg){ mssg$error<-errMssg }
  clearErrorMssg<-function(){ mssg$error<-"" }
  hasError<-reactive({ nchar(mssg$error)>0 })
  getErrorMssg<-reactive({ mssg$error })
  setCapturedMssg<-function(capturedMssg)({ 
    mssg$capturedOutput<-capturedMssg
  })
  getCapturedMssg<-reactive({ 
    mssg$capturedOutput
  })
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    updateAceExt(id= getAceEditorId(), sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue )
  }
  
  sendPtRManagerMessage<-function(sender, ...){ 
    cat('entering ---------------sendPtRManagerMessage---------------------\n')
    data<- c( list(sender=sender), list(...), list(fk=runif(1)))
    if(identical(sender,'tibNrow')){
      cat("Enter==============tibNRow data ======================\n")
      print(data)
      cat("Exit==============tibNRow data =======================\n")
    }
    lapply(data, function(dd){
      if(any(sapply(dd,is.na))){
        print(data)
        stop("encounterd an NA")
      }
    })
    session$sendCustomMessage( type = "ptRManager", data)
    cat('exiting ---------------sendPtRManagerMessage---------------------\n')
  }
  
  # sendFileTabsMessage<-function(id, sender, ...){ 
  #   cat( "sendFileTabsMessage:: id=",id," sender=",sender,"\n" )
  #   data<- c( list(value = id, sender=sender), list(...) )
  #   print(data)
  #   session$sendCustomMessage( type = "scrollManager", 
  #      c( list(value = id, sender=sender), list(...) )
  #   )
  # }
  
  sendFileTabsMessage<-function(...){ 
    #cat( "sendFileTabsMessage:: id=",id," sender=",sender,"\n" )
    data<- list(...) 
    #print(data)
    if(length(data)>0){
        if(identical(data$sender, 'savedStatus')){
          cat('sendFileTabsMessage\n')
          print(data)
        }
    #   lapply(data, function(x){
    #     if(is.null(x) || is.na(x) ){
    #       print(data)
    #       stop("encounterd an NA")
    #     }
    #   })
      session$sendCustomMessage( type = "scrollManager",  data )
    }
    
  }
  
  
  pages<- reactiveValues(
    fileName='',
    fileNameCount=1,
    tabIdCount=1
  )
  
  
  getNextAnonymousFileName<-function(){
    newFileName<-paste0("Anonymous ", pages$fileNameCount)
    pages$fileNameCount<-pages$fileNameCount+1
    #newTabId<-"source"
    #cat("getNextAnonymousFileName::newFileName",newFileName,"\n")
    newFileName
  }
  
  getNextTabId<-function(){
    tabId<-paste0("PTR-TABID", pages$tabIdCount)
    pages$tabIdCount<-pages$tabIdCount+1
    #newTabId<-"source"
    tabId
  }
  
  
  
  # tabName2AceId<-function(tabName){
  #   if(!is.null(tabName) && nchar(tabName)>0){
  #     tabName<-paste0("ACE", tabName)
  #     tabName<-gsub(' ','', tabName)
  #     tabName<-gsub('\\.','_',tabName)
  #   } else {
  #     NULL
  #   }
  # }
  # tabName2TabId<-function(tabName){
  #   if(!is.null(tabName) && nchar(tabName)>0){
  #     tabName<-paste0("TAB", tabName)
  #     tabName<-gsub(' ','', tabName)
  #     tabName<-gsub('\\.','_',tabName)
  #   } else {
  #     NULL
  #   }
  # }
  
  aceID2TabID<-function(aceId){
    sub("ACE","TAB",aceId)
  }
  tabID2aceID<-function(tabId){
    sub("TAB","ACE",tabId)
  }
  
  
  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  # extract points from user code
  getPtDefs<- reactive({ 
    if(is.null(getCode()) || nchar(getCode())==0){
      return(NULL)
    }  
    ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=editOption$useTribbleFormat ) 
    ptDefs
  })  
  
  
  shinyFileChoose(input, "buttonFileOpen",       session=session, roots=c(wd="~") ) #hidden
  shinyFileChoose(input, "buttonSnippetImport",    session=session, roots=c(wd="~"),  filetypes=c('snippets') ) #hidden
  shinyFileChoose(input, "buttonDnippetImport",    session=session, roots=c(wd="~"),  filetypes=c('dnippets') ) #hidden
  shinyFileSave(input,   "buttonExportSVG",      session=session, roots=c(wd="~")  ) #hidden
  
  # genShinySaveFilesServerConnection(input, session)
  # genShinySaveFilesObservers(input, session)
  # Reactive expressions------------- 
  showGrid<-reactive({displayOptions$showGrid})

  
  ptrDisplayScript =reactive({ 
    type=getRightMidPanel()
    if(type=='transform'){
      type=  paste0(type,".",getTransformType() )
    }
    scripts<-list(
      point=    'var ptRPlotter_ptR_SVG_Point = new PtRPanelPoints("ptR_SVG_Point");',
      value=    'var ptRPlotter_ptR_SVG_TagVal = new PtRPanelTagVal("ptR_SVG_TagVal");',
      transform.Translate= 'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");',
      transform.Rotate=    'var ptRPlotter_ptR_SVG_TRANSFORM_ROTATE = new PtRPanelRotate("ptR_SVG_TRANSFORM");',
      transform.Scale=     'var ptRPlotter_ptR_SVG_TRANSFORM_SCALE = new PtRPanelScale("ptR_SVG_TRANSFORM");',
      matrix=    'var ptRPlotter_ptR_SVG_TagDrag = new PtRPanelTagDrag("ptR_SVG_TagDrag");'
    )
    scripts[type]
  })

#------------------leftPanel--------------------------------
  source("leftPanel/mid/serverAce.R",                local=TRUE) 
  source("leftPanel/helpSVG.R",                      local=TRUE) 
  source("leftPanel/tabs/serverFileTabs.R",          local=TRUE) 
  source("leftPanel/fileIO/serverGenShinyFilesSaveObservers.R",        
                                                     local=TRUE)
  
#------------------rightPanel--------------------------------
  source("rightPanel/serverPlotSelectDB.R",          local=TRUE)
  source("rightPanel/footer/serverFooterRight.R",    local=TRUE) 
  source("rightPanel/header/serverEdTib.R",          local=TRUE)
  source("rightPanel/header/serverEdTransform.R",    local=TRUE)
  source("rightPanel/header/serverEdAsset.R",        local=TRUE)
  source("rightPanel/mid/serverRowDND.R",            local=TRUE)
  source("rightPanel/mid/serverPlotBarPoints.R",     local=TRUE) 
  source("rightPanel/mid/serverPlotBarTagValues.R",  local=TRUE)  
  source("rightPanel/mid/serverPlotBarTagDrag.R",    local=TRUE)  
  source("rightPanel/mid/serverPlotBarTransform.R",  local=TRUE) 
  source("rightPanel/mid/serverLog.R",               local=TRUE) 
  source("rightPanel/mid/serverPlotRmd.R",           local=TRUE) 
  source("rightPanel/mid/serverMouseClicks.R",       local=TRUE)
  source("rightPanel/menu/cmdNewColumn.R",           local=TRUE)
  source("rightPanel/menu/cmdNewAsset.R",            local=TRUE)
  source("rightPanel/menu/cmdSetMatColMax.R",        local=TRUE)
  source("rightPanel/menu/cmdDeleteColumn.R",        local=TRUE)
  source("rightPanel/menu/serverPlotBar.R",          local=TRUE)
  source("rightPanel/serverPanelCoordinator.R",      local=TRUE)
  source("rightPanel/serverPanelDispatch.R",         local=TRUE)
  source("rightPanel/serverOptions.R",               local=TRUE) 
  source("rightPanel/serverHandler.R",               local=TRUE)
  source("rightPanel/serverDisplayOptions.R",        local=TRUE)
  source("rightPanel/serverSelection.R",             local=TRUE)
  
#---------------leftPanel--------------------------
  source("leftPanel/footer/processCommit.R",        local=TRUE)
  source("leftPanel/footer/processKnit.R",          local=TRUE)
  source("leftPanel/footer/processDnip.R",          local=TRUE)
  source("leftPanel/footer/serverButtons.R",        local=TRUE)
  source("leftPanel/toolbar/cmdHToolBar.R",         local=TRUE)    
  source("leftPanel/menu/cmdFileSaveAs.R",          local=TRUE)  
  source("leftPanel/menu/cmdFileSave.R",            local=TRUE)  
  source("leftPanel/menu/cmdFileNew.R",             local=TRUE)  
  source("leftPanel/menu/cmdFileClose.R",           local=TRUE)  
  source("leftPanel/menu/cmdFileOpen.R",            local=TRUE)  
  source("leftPanel/menu/cmdFileQuit.R",            local=TRUE)  
  source("leftPanel/menu/cmdFileExportSvg.R",       local=TRUE) 
  source("leftPanel/menu/cmdOptionsTheme.R",        local=TRUE)
  source("leftPanel/menu/cmdOptionsFontSize.R",     local=TRUE)  
  source("leftPanel/menu/cmdFileSnippet.R",         local=TRUE)
  source("leftPanel/toolbar/serverDnippetToolBar.R",   local=TRUE)
  source("leftPanel/menu/cmdFileDnippet.R",         local=TRUE)
  source("leftPanel/menu/cmdAbout.R",               local=TRUE)
  source("leftPanel/serverEditBar.R",               local=TRUE)
})
