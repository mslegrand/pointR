processMssgFromAceMssgPageOut<-function(sender, mssg){
  id<-mssg$id
  saved<-mssg$isSaved
  if( !saved || sender %in% c('fileCmd.saveAs','fileCmd.saveNow', 'buttonCmd.rmdViewer') ) { #need to save
    docFilePath<-unlist(mssg$docFilePath)
    if(docFilePath=='?' || sender=='fileCmd.saveAs'){ # file unnamed : fileSaveAs
      tabId<-aceID2TabID(id)
      
      if(identical(request$mode, 'ptr')){
        ext=list(R='R')
      } else if( identical(request$mode, 'ptrrmd') ){
        ext=list(Rmd='Rmd')
      } else if( identical(request$mode, 'snippets') ){
        ext=list(snippet='snippets')
      } else if( identical(request$mode, 'dnippets') ){
        ext=list(dnippets='dnippets')
      } else {
        stop('unknowMode', format(request$mode))
        ext=list(text='txt')
      }
      ext<-shinyFiles:::formatFiletype(ext)
      
      target<-saveButtonFileNames[[request$mode]]
      sendPtRManagerMessage( sender=sender, saveFile=TRUE,  target=target, tabId=tabId )
    } else { 
      # write file
      code<-mssg$code
      # !!!TODO!!! if write fails revert.
      writeLines(code, docFilePath)
      
      updateAceExt(id, sender,  setDocFileSaved=TRUE)
      editOption$.saved<-TRUE
      tabId<-popTab()
      if(request$sender %in% c('fileCmd.close', 'fileCmd.quit')){
        addToRecentFiles(mssg$docFilePath)
        closeTabNow(tabId)
      } else { 
        if( identical(request$sender, 'buttonCmd.rmdViewer')){
          
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
    if(request$sender%in% c('fileCmd.close', 'fileCmd.quit') ){
      addToRecentFiles(mssg$docFilePath)
      closeTabNow(tabId)
    }
  }
}
