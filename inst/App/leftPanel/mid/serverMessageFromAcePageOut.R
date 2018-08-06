processMssgFromAceMssgPageOut<-function(sender, mssg){
  id<-mssg$id
  saved<-unlist(mssg$isSaved)
 
  if( !saved || sender %in% c('fileCmd.saveAs','fileCmd.saveNow', 'buttonCmd.rmdViewer', 'fileCmd.quit') ) { #need to save
    docFilePath<-unlist(mssg$docFilePath)
    if(docFilePath=='?' || sender=='fileCmd.saveAs'){ # file unnamed : fileSaveAs
      tabId<-aceID2TabID(id)
      ext<-mode2pathExt( getMode() )
      ext<-shinyFiles:::formatFiletype(ext)
      target<-saveButtonFileNames[[getMode()]]
      sendPtRManagerMessage( sender=sender, saveFile=TRUE,  target=target, tabId=tabId )
    } else { 
      # write file
      
      code<-mssg$code
      # !!!TODO!!! if write fails revert.
      writeLines(code, docFilePath)
      tabId<-aceID2TabID(id) 
      setFileDescSaved(tabId, TRUE)
      updateAceExt(id, sender,  setDocFileSaved=TRUE)
      editOption$.saved<-TRUE
      setFileDescSaved(pageId=tabId, fileSaveStatus=TRUE )
      savePage(tabId)
      if(sender %in% 'fileCmd.quit'){
       # pop off tab and exit from this
        tabId=popTab()
        
      } else if (sender %in% c('fileCmd.close')){
        addToRecentFiles(mssg$docFilePath)
        closeTabNow(tabId)
      } else { 
        if( identical(sender, 'buttonCmd.rmdViewer')){
          
          rmarkdown::render(docFilePath )
          htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
          browseURL(htmlPath)
        }
        addToRecentFiles(mssg$priorFilePath)
        title=as.character(tabTitleRfn( tabName=basename(docFilePath ), tabId=tabId, docFilePath=docFilePath ))
        sendFileTabsMessage(title=title, tabId=tabId)
      }
    }
  } else { #already saved
    if( identical(request$sender, 'buttonCmd.rmdViewer')){
      rmarkdown::render(docFilePath )
      htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
      browseURL(htmlPath)
    }
    tabId<-popTab()
    if(request$sender%in% c('fileCmd.close') ){
      addToRecentFiles(mssg$docFilePath)
      closeTabNow(tabId)
    }
  }
}
