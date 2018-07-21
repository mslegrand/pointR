
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
