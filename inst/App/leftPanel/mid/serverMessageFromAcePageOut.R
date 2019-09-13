
rmdOut<-function(docFilePath){
  # rmarkdown::render(docFilePath )
  if(usingElectron==TRUE){
    docFilePath<-gsub('~',homeDir,docFilePath)
    # TODO:: add check for Pandoc!!! 
    rmarkdown::render(docFilePath )
    href<-sub('\\.Rmd$','\\.html',docFilePath)
    href<-paste0('file://',href)
    
    sendPtRManagerMessage(sender='cmd.electron',  openLink= href)
  } else {
   
    rmarkdown::render(docFilePath )
    htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
    browseURL(htmlPath)
  }
  
}


processMssgFromAceMssgPageOut<-function(sender, mssg){
  id<-mssg$id
  saved<-unlist(mssg$isSaved)
  log.fin(processMssgFromAceMssgPageOut)
  
  reqCmd<-peekTabCmd()
  tabId<-aceID2TabID(id) # this should be the save as peekTabRequest
  docFilePath<-getFileDescriptor(tabId )$filePath
  if(docFilePath!="?" && (sender=='fileCmd.saveAs' || !saved ) ){ # saving
   
    code<-mssg$code  # !!!TODO!!! if write fails revert.
    writeLines(code, docFilePath)
    tabId<-aceID2TabID(id)
    
    # handle mode change
    oldeMode<-getMode()
    modeFromPath<-pathExt2mode(tools::file_ext(docFilePath)) # ---- reset mode if has changed! 
    updateAceExt(id, sender,  setDocFileSaved=TRUE, setMode=modeFromPath)  # resets undomanger, and possibly mode, but doesn't return anything
    setFileDescSaved(pageId=tabId, fileSaveStatus=TRUE ) # save status
    sendFileTabsMessage( tabId=tabId, sender='savedStatus', savedStatus='saved')
    
    
    if(!identical(oldeMode,modeFromPath)){ #mode has changed
      setFileDescMode(pageId=tabId,newMode=modeFromPath) # update mode in descriptor
      if(identical(modeFromPath,'ptr') && !(reqCmd %in% c('fileCmd.close','fileCmd.quit'))){
        tibs<-getPtDefs()$tib
        if(length(names(tibs))>0){
          name<-tail(names(tibs))
          resetSelectedTibbleName(tibs=tibs, name=name)
          storeAssetState()
          selectionList<-reactiveValuesToList(selectedAsset, all.names=TRUE) # !!! WFT is this??? not used???
        }
        processCommit() # this sets the sourceType
        reOrgPanels(id=mssg$id, mode= getModeX() )
        storeAssetState()
      }
      setTrigger('redraw')
      
     
    } #end of mode changeR
    # if mode is 'dnippets' refreshDnd
    ptRproj<-pprj()
    # browser()
    cat('modeFromPath=', format(modeFromPath),"\n")
    cat('ptRproj$pathToProj=',format(ptRproj$pathToProj),"\n")
    if(identical(modeFromPath,'dnippets') && !is.null(ptRproj$pathToProj) && dir.exists( ptRproj$pathToProj )){
      dndsDir<-path_join(c(ptRproj$pathToProj, resourceDir, 'dnds'))
      cat('dndsDir=',format(dndsDir),"\n")
      reloadDndDir(dndsDir)
    }
    
    savePage(tabId) # saves page to workspace
    if(sender=='fileCmd.saveAs'){
      addToRecentFiles(mssg$docFilePath)
      title=as.character(tabTitleRfn( tabName=basename(docFilePath ), tabId=tabId, docFilePath=docFilePath ))
      cat( "tabName=",basename(docFilePath ),"\n")
      sendFileTabsMessage(title=title, tabId=tabId)
    } 
    # else {
    #   sendFileTabsMessage( tabId=tabId, sender='savedStatus', savedStatus='saved')
    # }
    #popTabRequest()
  } #end of saving
  # final clean up
  
  if( identical(reqCmd , 'fileCmd.quit') && length(request$tabs)<=1){ 
    cmdQuitNow()
  } else if( identical(reqCmd , 'fileCmd.runApp') && length(request$tabs)<=1 ){
    app2RunPath<-getFileDescriptor(appRunner$tabId)$filePath
    sendPtRManagerMessage(sender='cmd.electron', app2RunPath=app2RunPath, tabId= appRunner$tabId)
  } else if( identical(reqCmd , 'fileCmd.close')){
    tabId=popTabRequest()
    if(mssg$docFilePath!="?"){
      addToRecentFiles(mssg$docFilePath)
    }
    closeTabNow(tabId)
  }  else if( identical(reqCmd , 'buttonCmd.rmdViewer') && length(request$tabs)<=1){
    tabId=popTabRequest()
    rmdOut(docFilePath)
  } else { #{if(reqCmd %in% c('fileCmd.save','fileCmd.saveAs'))
    tabId=popTabRequest()
  } 
}  
  
