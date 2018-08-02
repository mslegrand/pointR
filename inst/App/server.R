
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {

# Reactive values----------
  
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    updateAceExt(id= getAceEditorId(), sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue )
  }
  
  
  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  # extract points from user code
  getPtDefs<- reactive({ 
    if(is.null(getCode()) || nchar(getCode())==0){
      return(NULL)
    }  
    useTribbleFormat<- getUseTribble()
    #cat('useTribbleFormat=',format(useTribbleFormat),"\n")
   # ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=editOption$useTribbleFormat ) 
    ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=useTribbleFormat) 
    ptDefs
  })  
  
  
  shinyFileChoose(input, "buttonFileOpen",           session=session, roots=c(wd="~") ) #hidden
  shinyFileChoose(input, "buttonSnippetImport",      session=session, roots=c(wd="~"),  filetypes=c('snippets') ) #hidden
  shinyFileChoose(input, "buttonDnippetImport",      session=session, roots=c(wd="~"),  filetypes=c('dnippets') ) #hidden
  shinyFileChoose(input, "buttonPreProcPtImport",    session=session, roots=c(wd="~"),  filetypes=c('preprocpts') ) #hidden
  shinyFileSave(input,   "buttonExportSVG",          session=session, roots=c(wd="~")  ) #hidden
  shinyFileSave(input,   "buttonExportPreproc",      session=session, roots=c(wd="~")  ) #hidden
  



#------------------leftPanel--------------------------------
  source("leftPanel/serverRequest.R",                            local=TRUE) 
  source("leftPanel/serverSendPtRManagerMessage.R",              local=TRUE) 
  source("leftPanel/serverOutputMssg.R",                         local=TRUE) 
  source("leftPanel/mid/serverAce.R",                            local=TRUE) 
  source("leftPanel/mid/serverSendMessage2Ace.R",                local=TRUE) 
  source("leftPanel/mid/serverMessageFromAcePageIn.R",           local=TRUE) 
  source("leftPanel/mid/serverMessageFromAcePageOut.R",          local=TRUE) 
  source("leftPanel/helpSVG.R",                                  local=TRUE)
  source("leftPanel/tabs/serverFileTabs.R",                      local=TRUE) 
  
  
  #------------------fileIO----------------------------------
  source("fileIO/serverGenShinyFilesSaveObservers.R",            local=TRUE)
  source("fileIO/serverFileDescriptor.R",                        local=TRUE)
  
#------------------rightPanel--------------------------------
  source("rightPanel/selector/serverAssetSelectionDB.R",         local=TRUE)
  source("rightPanel/preProc/serverPreProcDB.R",                 local=TRUE)
  source("rightPanel/preProc/serverPreProcPtsInsert.R",          local=TRUE)
  source("rightPanel/preProc/serverPreProcPtsMove.R",            local=TRUE)
  source("rightPanel/preProc/serverPreProcMatMove.R",            local=TRUE)
  source("rightPanel/footer/serverFooterRight.R",                local=TRUE) 
  source("rightPanel/preProc/serverPreProcPts.R",                local=TRUE)
  source("rightPanel/header/serverEdTib.R",                      local=TRUE)
  source("rightPanel/header/serverEdTransform.R",                local=TRUE)
  source("rightPanel/header/serverEdAsset.R",                    local=TRUE)
  source("rightPanel/mid/serverRowDND.R",                        local=TRUE)
  source("rightPanel/mid/serverPlotBarPoints.R",                 local=TRUE) 
  source("rightPanel/mid/serverPlotBarTagValues.R",              local=TRUE)  
  source("rightPanel/mid/serverPlotBarTagDrag.R",                local=TRUE)  
  source("rightPanel/mid/serverPlotBarTransform.R",              local=TRUE) 
  source("rightPanel/mid/serverLog.R",                           local=TRUE) 
  source("rightPanel/mid/serverPlotRmd.R",                       local=TRUE) 
  source("rightPanel/mouse/serverMouseCmdAddPt.R",               local=TRUE)
  source("rightPanel/mouse/serverMouseCmdMovePt.R",              local=TRUE)
  source("rightPanel/mouse/serverMouseCmdMoveMatrix.R",          local=TRUE)
  source("rightPanel/mouse/serverMouseCmdValue.R",               local=TRUE)
  source("rightPanel/mouse/serverMouseCmdTransform.R",           local=TRUE)
  source("rightPanel/mouse/serverMouseClicks.R",                 local=TRUE)
  source("rightPanel/menu/cmdNewColumn.R",                       local=TRUE)
  source("rightPanel/menu/cmdNewAsset.R",                        local=TRUE)
  
  source("rightPanel/menu/cmdDeleteColumn.R",                    local=TRUE)
  source("rightPanel/menu/cmdFileImportPreProc.R",               local=TRUE)
  source("rightPanel/menu/cmdFileExportPreProc.R",               local=TRUE)
  source("rightPanel/menu/cmdRemovePreProc.R",                   local=TRUE)
  source("rightPanel/menu/serverPlotBar.R",                      local=TRUE)
  source("rightPanel/serverPanelCoordinator.R",                  local=TRUE)
  source("rightPanel/serverPanelDispatch.R",                     local=TRUE)
  source("rightPanel/serverOptions.R",                           local=TRUE) 
  source("rightPanel/selector/serverWidgetHandler.R",            local=TRUE)
  source("rightPanel/serverDisplayOptions.R",                    local=TRUE)
  source("rightPanel/selector/serverAssetSelection.R",           local=TRUE)
  source("rightPanel/mid/serverSvgGrid.R",                       local=TRUE)
  source("rightPanel/mid/serverSvgBackdrop.R",                   local=TRUE)
  
#---------------leftPanel--------------------------
  source("leftPanel/footer/processCommit.R",                    local=TRUE)
  source("leftPanel/footer/useTribble.R",                       local=TRUE)
  source("leftPanel/footer/processKnit.R",                      local=TRUE)
  source("leftPanel/footer/processDnip.R",                      local=TRUE)
  source("leftPanel/footer/serverButtons.R",                    local=TRUE)
  source("leftPanel/toolbar/cmdHToolBar.R",                     local=TRUE)    
  source("leftPanel/menu/cmdFileSaveAs.R",                      local=TRUE)  
  source("leftPanel/menu/cmdFileSave.R",                        local=TRUE)  
  source("leftPanel/menu/cmdFileNew.R",                         local=TRUE)  
  source("leftPanel/menu/cmdFileClose.R",                       local=TRUE)  
  source("leftPanel/menu/cmdFileOpen.R",                        local=TRUE)  
  source("leftPanel/menu/cmdFileQuit.R",                        local=TRUE)  
  source("leftPanel/menu/cmdFileExportSvg.R",                   local=TRUE) 
  source("leftPanel/menu/cmdOptionsTheme.R",                    local=TRUE)
  source("leftPanel/menu/cmdOptionsFontSize.R",                 local=TRUE)  
  source("leftPanel/menu/cmdFileSnippet.R",                     local=TRUE)
  source("leftPanel/dnippets/serverDnippetToolBar.R",           local=TRUE)
  source("leftPanel/dnippets/serverDnippetCntrl.R",             local=TRUE)
  source("leftPanel/menu/cmdFileDnippet.R",                     local=TRUE)
  source("leftPanel/menu/cmdAbout.R",                           local=TRUE)
  source("leftPanel/menu/serverEditBar.R",                      local=TRUE)
})
