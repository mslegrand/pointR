
processMssgFromAceMssgPageIn<-function(sender, mssg){

    processCommit() # this sets the sourceType
    # cat("sender = ", format(sender),"\n")
    # cat("assetName = ", format(getAssetName()),"\n")
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
        #cat('storeAssetState\n')
        storeAssetState()
        #cat("restoreAssetState\n")
        restoreAssetState(input$pages)
        savePage(input$pages) # require for new page that was not committed
        
      } else{
        # cat('hello world\n')
        # cat( "length(input$pages)=" ,  length(input$pages), "\n")
        # cat( "format(input$pages)=" ,  format(tttid), "\n")
      }
      # end assetUpdate:
    }
}
