

observeEvent(input$messageFromAce, {
  # cat('\n====serverAce:...observe input$messageFromAce:: entering\n')
  # cat('\n initial value of getTibRow()=', format(getTibRow()), "\n")
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      clearErrorMssg()
      
      # cat("input$messageFromAce$id=" , format(input$messageFromAce$id), "\n")
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      # cat('request$sender=',format(request$sender),"\n")
      if(length(input$messageFromAce$isSaved)>0){ 
        aceId<-input$messageFromAce$id
        editOption$.saved <- input$messageFromAce$isSaved
        # cat('\n--setting editOption$.saved --\n')
        # cat("set editOption$.saved=",editOption$.saved,"\n")
      }
      # cat('22 ace request$sender=',format(request$sender),"\n")

      if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new', 'cmd.tabChange')){
        processMssgFromAceMssgPageIn(sender, input$messageFromAce)
      } 
      
      if( sender %in% c( 'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' , 'fileCmd.saveNow', 'buttonCmd.rmdViewer')){
        processMssgFromAceMssgPageOut(sender, input$messageFromAce) 
      }

      # cat('\n final value of getTibRow()=', format(getTibRow()), "\n")
    }
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE)


processMssgFromAceMssgPageIn<-function(sender, mssg){
  if(sender %in% c('cmd.tabChange', 'cmd.file.new', 'cmd.openFileNow')){
    #browser()
    request$mode<-mssg$mode
    # cat('Ace:: request$mode=', request$mode, '\n')
    if(identical(request$mode, 'ptrrmd')){
      panels$sourceType<-rmdPanelTag
      processCommit()
      return(NULL) #skip the rest of the processing (for ptR mode)
    } 
    if(identical(request$mode, 'dnippets')){
      panels$sourceType<-rmdPanelTag
      processCommit()
      return(NULL) #skip the rest of the processing (for ptR mode)
    } 
    if(identical(request$mode, 'text')){
      panels$sourceType<-textPanelTag
      processCommit()
      return(NULL) #skip the rest of the processing (for ptR mode)
    } 
    if(identical(request$mode, 'snippets')){
      panels$sourceType<-snippetPanelTag
      processCommit()
      return(NULL) #skip the rest of the processing (for ptR mode)
    } 
  }
  # From this point on, all processing assumes ptR mode
  if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new', 'cmd.tabChange')){
    # cat('33 request$sender=',format(request$sender),"\n")
    #cat('Ace: invoking processCommit\n')
    processCommit() # this sets the sourceType
    cat('mssgFromAce:: returning from processCommit\n')
    cat('mssgFromAce:: getAssetName()=',format(getAssetName()),"\n")
    if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow') && !is.null(getAssetName())){ 
      if(sender=='cmd.add.asset'){
        name=mssg$selector$assetName
      } else {
        name=getAssetName() # 'cmd.commit', 'cmd.add.column'
      }
      tibs<-getPtDefs()$tib
      # cat('name=',format(name),"\n")
      # cat("ace invoking resetSelectedTibbleName\n")
      resetSelectedTibbleName(tibs=tibs, name=name)
    } else { 
      #name=NULL #  'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new'
      # cat('else: ', 'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new',"\n")
      # cat('sender=',format(sender),"\n")
      # cat("length(input$pages)=",length(input$pages),"\n")
      tttid<-input$pages;
      # cat("input$pages=",format(tttid),"\n")
      if(length(input$pages) >0 && 
         nchar(input$pages)>0 && 
         selectedAsset$tabId != input$pages ){
        storeAssetState()
        restoreAssetState(input$pages)
      }
      # end selectTibUpdate:
      
    }
  }
}

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
        # cat('request$mode=',request$mode,"\n")
        stop('unknowMode')
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




updateAceExtDef<-function(newPtDef, sender, selector=list() ){
  if(!is.null(getCode())){
    newPtDef$tib<-pts2Integers(newPtDef$tib )
    replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() ) #name not used!!!
    if( length(replacementList)>0 ){
      data<-list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
      lapply(data, function(x){
        if(any(unlist(lapply(x, is.na )))){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage( type = "shinyAceExt", data )
    }
  }

}

updateAceExt<-function(id, sender, ... ){
  data<-list(...)
  if(is.null(sender)){stop('null sender')}
  if(length(data)>0){
    if(length(id)==0){
      id<-'bogus'
    }
    data<-c(list(id= id, sender=sender), data )
    # if(TRUE){
    #     cat("Entering updateAceExt::\n")
    #     cat("updateAceExt::id=",format(id), ", length(id)=",length(id),",class(id)=", class(id),"\n")
    #     cat("updateAceExt::sender=",format(sender),"\n")
    #     cat("updateAceExt::names(data)=",format(names(data)),"\n")
    #     cat("updateAceExt::data=",format(data),"\n")
    #     #print(data)
    #     cat("updateAceExt::sendCustomMessage NOW\n")
    # }
    if(length(id)>0 && nchar(id)>0){
      lapply(data, function(d){
        if(length(d)==0){
          cat("-----------\n")
          print(data)
          stop('d has length 0')
        }
        if(length(d)==1 && is.na(d)){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage(
        type = "shinyAceExt",
        data
      )
    }
    
  }
}

observeEvent(request$sender,{
    if(request$sender=='startup'){
      cmdFileNewPtR()
      sampleDnippets<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
      cat(sampleDnippets)
      loadDndSnippets(sampleDnippets)
    }
}, priority=100)

# TODO!!!: rewrite
updateSelected4Ace<-function( reqSelector){
  # TODO: make into iteration
  # updatEle<- c('name', 'ptColName', 'rowIndex', 'matCol', 'colName')
  cat('names of reqSelector:', paste(names(reqSelector), collapse=", "),"\n" )
  for(n in names(reqSelector)){
    stopifnot({n %in% names(selectedAsset)})
    selectedAsset[[n]]<-reqSelector[[n]]
  }
  # if(!is.null(reqSelector[['name']])){
  #   #cat("reqSelector$name=", format(reqSelector$name ),"\n")
  #   selectedAsset$name=reqSelector[['name']]
  # }
  # if(!is.null(reqSelector[['ptColName']])){
  #   #cat("reqSelector$ptColName=", format(reqSelector$ptColName ),"\n")
  #   selectedAsset$ptColName=reqSelector[['ptColName']]
  # }
  # if(!is.null(reqSelector[['rowIndex']])){ # !!! may want to provide a check here
  #   #cat("reqSelector$ptColName=", format(reqSelector$rowIndex ),"\n")
  #   selectedAsset$rowIndex=reqSelector[['rowIndex']]
  # }
  # if(!is.null(reqSelector[['matCol']])){
  #   #cat("reqSelector$matCol=", format(reqSelector$matCol ),"\n")
  #   selectedAsset$matCol=reqSelector[['matCol']]
  # }
  # if(!is.null(reqSelector[['columnName']])){
  #   #cat("reqSelector$columnName=", format(reqSelector$columnName ),"\n")
  #   selectedAsset$columnName=reqSelector[['columnName']]
  # }
} 

