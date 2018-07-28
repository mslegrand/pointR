
processMssgFromAceMssgPageIn<-function(sender, mssg){
  # if(sender %in% c('cmd.tabChange', 'cmd.file.new', 'cmd.openFileNow')){
  #   #browser()
  #   request$mode<-mssg$mode
  #   # cat('Ace:: request$mode=', request$mode, '\n')
  #   if(identical(request$mode, 'ptrrmd')){
  #     panels$sourceType<-rmdPanelTag
  #     processCommit()
  #     return(NULL) #skip the rest of the processing (for ptR mode)
  #   } 
  #   if(identical(request$mode, 'dnippets')){
  #     panels$sourceType<-rmdPanelTag
  #     processCommit()
  #     return(NULL) #skip the rest of the processing (for ptR mode)
  #   } 
  #   if(identical(request$mode, 'text')){
  #     panels$sourceType<-textPanelTag
  #     processCommit()
  #     return(NULL) #skip the rest of the processing (for ptR mode)
  #   } 
  #   if(identical(request$mode, 'snippets')){
  #     panels$sourceType<-snippetPanelTag
  #     processCommit()
  #     return(NULL) #skip the rest of the processing (for ptR mode)
  #   } 
  # }
  # At this point, either mode=='ptr' or sender is commit and mode is 'ptrrmd' or 'dnippets'
  #request$mode<-mssg$mode
  if(sender %in% c(
    'cmd.tabChange', 'cmd.file.new', 'cmd.openFileNow',
    'cmd.commit', 'cmd.add.column', 'cmd.add.asset' #,  'cmd.saveFileNow'  
  )){
    processCommit() # this sets the sourceType
    cat("sender = ", format(sender),"\n")
    cat("assetName = ", format(getAssetName()),"\n")
    if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset') && !is.null(getAssetName())){ 
      #if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow') && !is.null(getAssetName())){ 
      if(sender=='cmd.add.asset'){
        name=mssg$selector$assetName
      } else {
        name=getAssetName() # 'cmd.commit', 'cmd.add.column'
        # cat('getAssetName()=', format(getAssetName()), "\n")
      }
      tibs<-getPtDefs()$tib
      # cat('name=',format(name),"\n")
      # cat("ace invoking resetSelectedTibbleName\n")
      # cat('names of tibs:',format(names(tibs)), "!\n")
      resetSelectedTibbleName(tibs=tibs, name=name)
    } else { 
      
      tttid<-input$pages;
      if(length(input$pages) >0 && 
         nchar(input$pages)>0 && 
         !identical(selectedAsset$tabId, input$pages) 
      ){
        # cat('storeAssetState\n')
        storeAssetState()
        # cat("restoreAssetState\n")
        restoreAssetState(input$pages)
      } else{
        # cat( "length(input$pages)=" ,  length(input$pages), "\n")
        # cat( "format(input$pages)=" ,  format(tttid), "\n")
      }
      # end assetUpdate:
      
    }
  }
}
