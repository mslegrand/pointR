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
      sendPtRManagerMessage( sender=sender, saveFile=TRUE,  target=target, tabId=tabId ) #triggers shinyFiles
    } else { # has legitmate path
      # write file
      code<-mssg$code  # !!!TODO!!! if write fails revert.
      

      writeLines(code, docFilePath)
      tabId<-aceID2TabID(id) 
      
      oldeMode<-getMode()
      modeFromPath<-pathExt2mode(tools::file_ext(docFilePath)) # ---- reset mode if has changed! 
      
      updateAceExt(id, sender,  setDocFileSaved=TRUE, setMode=modeFromPath)  # resets undomanger, and possibly mode, but doesn't return anything
      setFileDescSaved(pageId=tabId, fileSaveStatus=TRUE ) # save status 
      if(!identical(oldeMode,modeFromPath)){
        
        setFileDescMode(pageId=tabId,newMode=modeFromPath) # update mode in descriptor
        if(identical(modeFromPath,'ptr') && !(sender %in% c('fileCmd.close','fileCmd.quit'))){
          tibs<-getPtDefs()$tib
          if(length(names(tibs))>0){
            name<-tail(names(tibs))
            resetSelectedTibbleName(tibs=tibs, name=name)
            storeAssetState()
            selectionList<-reactiveValuesToList(selectedAsset, all.names=TRUE)
            print(selectionList)
            # setTrigger('redraw')
          }
          
          # ptDefs<-ex.getPtDefs(code, useTribbleFormat=TRUE)
          # updateSelected(name=RPanelTag)
          #browser()
          # storeAssetState()
          # cat("--restoreAssetState\n")
          processCommit() # this sets the sourceType
          # cat('--reOrgPanels')
          reOrgPanels(id=mssg$id, mode= getMode() )
          storeAssetState()
            # browser()
          # restoreAssetState(input$pages) #copies from db to assetSelection
          
        }
        setTrigger('redraw')
      }
      savePage(tabId) # saves page to workspace
      if(sender %in% 'fileCmd.quit'){
       # pop off tab and exit from this
        tabId=popTab()
      } else if (sender %in% c('fileCmd.close')){ # if not saved and closing
        addToRecentFiles(mssg$docFilePath)
        closeTabNow(tabId)
      } else { # had path and is neither quit nor close nor saveAs; so is rmdViewer or saveNow
        if( identical(sender, 'buttonCmd.rmdViewer')){
          
          rmarkdown::render(docFilePath )
          htmlPath<-sub('\\.Rmd$','\\.html',docFilePath)
          browseURL(htmlPath)
        }
        addToRecentFiles(mssg$docFilePath)
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
