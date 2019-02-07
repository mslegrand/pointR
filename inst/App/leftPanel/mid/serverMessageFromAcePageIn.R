
processMssgFromAceMssgPageIn<-function(sender, mssg){
    # cat('>---> processMssgFromAceMssgPageIn\n')
    
    #reOrgPanels(id=mssg$id, mode= getMode() )
    # cat("sender = ", format(sender),"\n")
    # cat("assetName = ", format(getAssetName()),"\n")
    if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset') && !is.null(getAssetName())){ 
      #if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow') && !is.null(getAssetName())){ 
      processCommit() # this sets the sourceType
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
      
    } else { # else covers: 'cmd.file.new', 'cmd.tabChange', 'cmd.openFileNow', p

# getMode -----------------------------------------------------------------

      if(sender %in% c('cmd.file.new',  'cmd.openFileNow')){
        base::stop('ace sender=',sender,"\n")
      }
      # cat('hhh\n')
      if(length(input$pages) >0 && 
         nchar(input$pages)>0 && 
         !identical(selectedAsset$tabId, input$pages) 
      ){
        # browser()
         # cat('--storeAssetState\n')
        storeAssetState()
         # cat("--restoreAssetState\n")
        processCommit() # this sets the sourceType
        # cat('--reOrgPanels')
        reOrgPanels(id=mssg$id, mode= getMode() )
        restoreAssetState(input$pages) #copies from db to assetSelection
        # browser()
         # cat('--savePage\n')
        savePage(input$pages) # require for new page that was not committed
        sendFileTabsMessage(selected=input$pages, resize=runif(1)) 
        
      } else{ # case: length(input$pages)==0 || identical(selectedAsset$tabId, input$pages) ==TRUE
        
      }
      reOrgPanels(id=mssg$id, mode= getMode() ) 
      
      # end assetUpdate:
    } 
    setTrigger('redraw')
    # cat('<---< processMssgFromAceMssgPageIn\n\n')
}
