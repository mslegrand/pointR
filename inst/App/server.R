
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  # debugging tools
  # cat_list <- c()
  # exportTestValues(catList=cat_list)
  
# utility functions---------------
  setwd(path_join(c(ptRPath,"App")))
  cat("--------------begin server-------------\ngetwd()=",getwd(),"\n")
  source("util/dbInitiatizers.R") 
  
  source("util/format.R")
  source("util/utilParser.R")
  source("util/utilptR.R", local=TRUE)
  source("util/utilTibble.R")
  source("util/utilColumnType.R")
  source("util/utilTransform.R")
  source("util/copyAndRenameProject.R")
  source("fileIO/dndSnippetLoader.R")
  source("rightPanel/preProc/preProcSetAttr.R")
  source("rightPanel/preProc/preprocTrySetAttrValue.R", local=TRUE)
  source("rightPanel/preProc/preProcValidate.R")
  
# Reactive values----------
  source("util/reOrgPanels.R", local=TRUE ) 
  reOrgPanels(id=NULL, mode=NULL)
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    updateAceExt(id= getAceEditorId(), sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue )
  }
  
  
  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  # extract points from user code
  #getPtDefs<- eventReactive(c(getCode(),input$pages), { # may need to add tab change??? or change back to reactive
  getPtDefs<- reactive({
    if(is.null(getCode()) || nchar(getCode())==0 || !identical(getMode(), 'ptr')  ){
      return(NULL)
    }  
    useTribbleFormat<- getUseTribble()
    ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=useTribbleFormat) 
    ptDefs
  })  
  

  
  disableDMDM(session, "editNavBar", 'project')
  

#------------------leftPanel--------------------------------
 
  source("leftPanel/serverRequest.R",                            local=TRUE) 
  source("leftPanel/serverSendPtRManagerMessage.R",              local=TRUE) 
  source("leftPanel/serverOutputMssg.R",                         local=TRUE) 
  source("leftPanel/mid/serverAce.R",                            local=TRUE) 
  source("leftPanel/mid/serverSendMessage2Ace.R",                local=TRUE) 
  source("leftPanel/mid/serverMessageFromAcePageIn.R",           local=TRUE) 
  source("leftPanel/mid/serverMessageFromAcePageOut.R",          local=TRUE) 
  source("leftPanel/helpSVG.R",                                  local=TRUE)
  source("leftPanel/tabs/serverNewPage.R",                       local=TRUE)
  source("leftPanel/tabs/serverFileTabs.R",                      local=TRUE) 
  
  
  #------------------fileIO----------------------------------
  source("fileIO/serverShinyFiles.R",                            local=TRUE) 
  source("fileIO/serverGenShinyFilesSaveObservers.R",            local=TRUE)
  source("fileIO/serverFileDescriptor.R",                        local=TRUE)
  source("fileIO/serverLoadWorkSpace.R",                         local=TRUE)
  source("fileIO/serverPage2Workspace.R",                        local=TRUE)
  
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
  source("rightPanel/mid/serverPlotBarSVG.R",                    local=TRUE) 
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
  source("rightPanel/serverOptions.R",                           local=TRUE) # set initially by copying from configIO
  source("fileIO/currentTabIO.R",                                local=TRUE)
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
  source("leftPanel/menu/cmdFileOpenProject.R",                 local=TRUE)  
  source("leftPanel/menu/cmdFileOpen.R",                        local=TRUE)  
  source("leftPanel/menu/cmdFileQuit.R",                        local=TRUE) 
  
  source("leftPanel/pproj/pprojUtil.R",                         local=TRUE)  
  source("leftPanel/pproj/pprojNew.R",                          local=TRUE)  
  source("leftPanel/pproj/pprojModalClone.R",                   local=TRUE)  
  source("leftPanel/pproj/pprojModalNewCntrl.R",                   local=TRUE)  
  
  source("leftPanel/pproj/pprojOpen.R",                         local=TRUE)   
  source("leftPanel/pproj/pprojClose.R",                        local=TRUE)  
  source("leftPanel/pproj/pprojModalNew.R",                     local=TRUE)  
  
  source("leftPanel/menu/cmdFileExportSvg.R",                   local=TRUE) 
  source("leftPanel/menu/cmdOptionsTheme.R",                    local=TRUE)
  source("leftPanel/menu/cmdOptionsFontSize.R",                 local=TRUE)  
  source("leftPanel/menu/cmdFileSnippet.R",                     local=TRUE)
  source("leftPanel/dnippets/serverDnippetCntrl.R",             local=TRUE)
  source("leftPanel/dnippets/serverDnippetToolBar.R",           local=TRUE)
  source("leftPanel/dnippets/serverDnippetsDB.R",               local=TRUE)
  source("leftPanel/dnippets/serverSaveDnippets.R",             local=TRUE)
  source("leftPanel/menu/cmdFileDnippet.R",                     local=TRUE)
  source("leftPanel/menu/cmdAbout.R",                           local=TRUE)
  source("leftPanel/menu/serverEditBar.R",                      local=TRUE)
  source("fileIO/observeRequestStartUp.R",                      local=TRUE)
  


  # exportTestValues( request.sender=request$sender)
  # exportTestValues(request.code=request$code)
  # exportTestValues(selectedAsset.tabId=selectedAsset$tabId)
  if(usingElectron){
    sendPtRManagerMessage(sender='cmd.electron', version=version)
    disable("stopShinyApp")
  } 
  
  
})
