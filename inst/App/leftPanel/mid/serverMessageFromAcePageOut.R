
rmdOut<-function(docFilePath){
  # rmarkdown::render(docFilePath )
  if(usingElectron==TRUE){
    docFilePath<-gsub('~',homeDir,docFilePath)
    rmarkdown::render(docFilePath )
    href<-sub('\\.Rmd$','\\.html',docFilePath)
    href<-paste0('file://',href)
    cat('href=',href,"\n")
    sendPtRManagerMessage(sender='cmd.electron',  openLink= href)
  } else {
    cat('no electron\n')
    rmarkdown::render(docFilePath )
    htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
    browseURL(htmlPath)
  }
  
}


processMssgFromAceMssgPageOut<-function(sender, mssg){
  id<-mssg$id
  saved<-unlist(mssg$isSaved)
  # some confusion
  #  fileCmd.save vs fileCmd.saveNow vs fileCmd.saveAs
  #  the request$sender is populated (sender+tabs) by either
  #    1. "fileCmd.save" (by menu cmds cmdFileSave or cmdFileSaveAll)
  #    2. "fileCmd.saveAs" (by menu cmds cmdFileSaveAs)
  #  "fileCmd.saveNow" is 
  #    1. issued by fileSaveObserver directly to ace, so name is established by a saveButtonFile dialog
  #    2. saveButtonFile dialog is triggered by ace sending to ptRManager a saveFile and tabId
  #    3. ace sends (#2) to ptManager only upon 
  #          a. when receiving "fileCmd.saveAs" or 
  #          b. receiving a tabId with ? for docFilePath and for sender that is ...
  #    4. 
  
  log.fin(processMssgFromAceMssgPageOut)
  
  if( !saved || sender %in% c('fileCmd.save', 'fileCmd.saveAs','fileCmd.saveNow', 'buttonCmd.rmdViewer', 'fileCmd.quit') ) { #need to save
    docFilePath<-unlist(mssg$docFilePath)
    if(docFilePath=='?' || sender=='fileCmd.saveAs'){ # file unnamed : fileSaveAs
      tabId<-aceID2TabID(id)
      
      ext<-mode2pathExt( getMode() )
      ext<-shinyFiles:::formatFiletype(ext)
      target<-saveButtonFileNames[[getMode()]]
      sendPtRManagerMessage( sender=sender, saveFile=TRUE,  target=target, tabId=tabId ) # triggers shinyFiles
    } else { # has legitmate path:: docFilePath!='?' && sender in c('fileCmd.save', 'fileCmd.saveNow', 'buttonCmd.rmdViewer', 'fileCmd.quit')
      # write file
      code<-mssg$code  # !!!TODO!!! if write fails revert.
      
      writeLines(code, docFilePath)
      tabId<-aceID2TabID(id) 
      
      oldeMode<-getMode()
      modeFromPath<-pathExt2mode(tools::file_ext(docFilePath)) # ---- reset mode if has changed! 
      
      updateAceExt(id, sender,  setDocFileSaved=TRUE, setMode=modeFromPath)  # resets undomanger, and possibly mode, but doesn't return anything
      setFileDescSaved(pageId=tabId, fileSaveStatus=TRUE ) # save status
      
      # handle mode status change
      if(identical(oldeMode,modeFromPath)){
        modeChanged<-FALSE
      } else {
        modeChanged<-TRUE # mode was changed
        setFileDescMode(pageId=tabId,newMode=modeFromPath) # update mode in descriptor
        if(identical(modeFromPath,'ptr') && !(sender %in% c('fileCmd.close','fileCmd.quit'))){
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
      }
      
      savePage(tabId) # saves page to workspace
      if(sender %in% c('fileCmd.save','fileCmd.saveNow')){
        tabId=popTab()
      }
      if(sender %in% 'fileCmd.quit'){
       # pop off tab and exit from this
        tabId=popTab()
      } else if (sender %in% c('fileCmd.close')){ # if not saved and closing
        addToRecentFiles(mssg$docFilePath)
        closeTabNow(tabId)
      } else { # had path and is neither quit nor close nor saveAs; so is rmdViewer or saveNow
        if( identical(sender, 'buttonCmd.rmdViewer')){
          cat('aceOut1:: buttonCmd.rmdViewer\n')
          rmdOut(docFilePath)
          # rmarkdown::render(docFilePath )
          # htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
          # browseURL(htmlPath)
        } else { #was save Now
          if(modeChanged){
            updateAceExt(id=id, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
          }
        }
        
        addToRecentFiles(mssg$docFilePath)
        title=as.character(tabTitleRfn( tabName=basename(docFilePath ), tabId=tabId, docFilePath=docFilePath ))
        sendFileTabsMessage(title=title, tabId=tabId)
      }
    }
  } else { #already saved
    if( identical(request$sender, 'buttonCmd.rmdViewer')){
      rmdOut(docFilePath)
      cat('aceOut1:: buttonCmd.rmdViewer\n')
      # rmarkdown::render(docFilePath )
      # htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
      # browseURL(htmlPath)
    }
    tabId<-popTab()
    if(request$sender%in% c('fileCmd.close') ){
      addToRecentFiles(mssg$docFilePath)
      closeTabNow(tabId)
    }
  }
  log.fout(processMssgFromAceMssgPageOut)
}
