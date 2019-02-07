
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  cat_list <- c()
  exportTestValues(catList=cat_list)
  
# utility functions---------------
  source("util/dbInitiatizers.R") 
  source("util/extNmode.R") 
  source("util/format.R")
  source("util/utilParser.R")
  source("util/utilptR.R")
  source("util/utilTibble.R")
  source("util/utilColumnType.R")
  source("util/utilTransform.R")
  source("util/copyAndRenameProject.R")
  source("fileIO/dndSnippetLoader.R")
  
# Reactive values----------
  source("util/reOrgPanels.R", local=TRUE ) 
  reOrgPanels(id=NULL, mode=NULL)
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    # cat(">---> triggerRefresh\n")
    updateAceExt(id= getAceEditorId(), sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue )
    # cat("<---< triggerRefresh\n")
  }
  
  
  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  # extract points from user code
  #getPtDefs<- eventReactive(c(getCode(),input$pages), { # may need to add tab change??? or change back to reactive
  getPtDefs<- reactive({
    if(is.null(getCode()) || nchar(getCode())==0 || !identical(getMode(), 'ptr')  ){
      return(NULL)
    }  
    # browser()
    useTribbleFormat<- getUseTribble()
    ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=useTribbleFormat) 
    ptDefs
  })  
  

  shinyFileChoose(input, "buttonFileOpen",           session=session, roots=c(home="~"),  filetypes=c('R','PTR','SVGR','js') ) #hidden
  shinyFileChoose(input, "buttonFileOpenProject",    session=session, roots=c(home="~"),  filetypes=c('pprj') ) #hidden
  shinyFileChoose(input, "buttonSnippetImport",      session=session, roots=c(home="~"),  filetypes=c('snippets') ) #hidden
  shinyFileChoose(input, "buttonDnippetImport",      session=session, roots=c(home="~"),  filetypes=c('dnippets') ) #hidden
  shinyFileChoose(input, "buttonPreProcPtImport",    session=session, roots=c(home="~"),  filetypes=c('preprocpts') ) #hidden
  shinyFileSave(input,   "buttonExportSVG",          session=session, roots=c(home="~")  ) #hidden
  shinyFileSave(input,   "buttonExportPreproc",      session=session, roots=c(home="~") ) #hidden
  
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
  
  # tmpVars<-c("%AND%", "assertConsistantTibPair", "bar1", "bar2", "buildHToolBar", 
  #            "buildLeftMenu", "buildRightMenu", "charColType", "choices2ColType", 
  #            "codeTemplate", "colLine2charPos", "colLine2charPositions", "defaultOpts", 
  #            "defTag", "dnippetsDirPath", "drippetdirectoryPath", "dripplets2List", 
  #            "dripplets2List2", "dripplets2Rmd", "errorPanelTag", "ex.getPtDefs", 
  #            "extMode.TB", "extractColType", "extractColumnIndex", "extractDripplet", 
  #            "extractPositions", "extractSafeRowColIndex", "extractTagDF", 
  #            "extractVal", "fileTemplates", "fileTemplatesNames", "fmtMat", 
  #            "fmtTibble", "fmtTibbleList", "fmtTribble", "genShinyOpenFilesButtons", 
  #            "genShinySaveFilesButtons", "getcumCharLines", "getDef", "getDefPos", 
  #            "getExtenstionList", "getNCharLines", "getParseDataFrame", "getTibColClass", 
  #            "imageBlockIndices", "initialBackDropDB", "initialFileDescDB", 
  #            "initialPreprocDB", "initialPtrAceOptions", "initialServerAsset", 
  #            "initialServerAssetDB", "initialSvgGridDB", "initialTribbleDB", 
  #            "initResourcePaths", "isBooleanString", "isColorString", "isInteger", 
  #            "isIntegerString", "isNumericString", "isPercentageString", "isPoints", 
  #            "list.entry.at.index", "listColType", "lowerBd", "mode2pathExt", 
  #            "modes2filetypes", "moduleEdAsset", "moduleEdAssetUI", "moduleEdTib", 
  #            "moduleEdTibUI", "moduleEdTransform", "moduleEdTransformUI", 
  #            "moduleFooterRight", "moduleFooterRightUI", "moduleLog", "moduleLogUI", 
  #            "modulePlotSVGr", "modulePlotSVGrUI", "moduleRowDND", "moduleRowDNDUI", 
  #            "newPointPreprocessor", "optionDirPath", "optionFile", "pathExt2mode", 
  #            "preprocChoices", "ptDef2ReplacementList", "pts2Integers", "r_pkgs", 
  #            "readOptionsJSON", "readTemplate", "rmdPanelTag", "row2DrippletBlock", 
  #            "row2DrippletBlockIndices", "RPanelTag", "saveButtonFileNames", 
  #            "shinyAce4Ptr", "snippetPanelTag", "snippetsDirPath", "stop.unless", 
  #            "svgPanelTag", "svgToolsScript", "tagTib", "textPanelTag", "tibTag", 
  #            "tid2replacementCoord", "toggleTabType", "toStrPtR0", "toStrPtR0.character", 
  #            "toStrPtR0.default", "toStrPtR0.list", "toStrPtR0.matrix", "toStrPtR0.numeric", 
  #            "transformTag", "type2ExtensionList", "upperBd", "usingDraggable", 
  #            "val2ColType", "validateTibLists", "version", "versionCheck", 
  #            "writeOptionsJSON")
  #on.exit(rm(list=list(tmpVars)))
  
  exportTestValues( request.sender=request$sender)
  exportTestValues(request.code=request$code)
  exportTestValues(selectedAsset.tabId=selectedAsset$tabId)
  
})
