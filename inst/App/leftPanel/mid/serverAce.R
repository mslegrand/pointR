
observeEvent(input$messageFromAce, {
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      aceId<-input$messageFromAce$id
      tabId<-aceID2TabID(aceId)
      clearErrorMssg()
      cat('messageFromAce:: sender=',sender,'\n')
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        setSelectedAssetFromAce(reqSelector)
      }
      if(length(input$messageFromAce$docFilePath)>0 ){
        docFilePath<-unlist(input$messageFromAce$docFilePath)
        #tabId<-aceID2TabID(aceId)
        cat( "docFilePath=",docFilePath,"\n")
        setFileDescPath(tabId, docFilePath)
        savePage(tabId)
      }
      if(length(input$messageFromAce$isSaved)>0){ 
        cat("input$messageFromAce$isSaved=" , input$messageFromAce$isSaved ,"\n")
        cat("class(input$messageFromAce$isSaved)=" , class(input$messageFromAce$isSaved) ,"\n")
        #tabId<-aceID2TabID(aceId)
        # if(identical(getFileDescriptor(tabId)$filePath,"?")){
        #   editOption$.saved <-'FALSE'
        # } else {
          editOption$.saved <- input$messageFromAce$isSaved
        # }
        setFileDescSaved(tabId, input$messageFromAce$isSaved)
        #editOption$.saved <- input$messageFromAce$isSaved
        
        #setFileDescSaved(tabId, input$messageFromAce$isSaved)
        savePage(tabId)
      }
      
     
      
      if(
        sender %in% c( 
          'cmd.file.new', 'cmd.tabChange', 'cmd.openFileNow', 
          'cmd.commit', 'cmd.add.column', 'cmd.add.asset' #, 'cmd.saveFileNow' 
          )
      ){#not sure if cmd.saveFileNow should be here, infact, cannot find sender issuing this.
        processMssgFromAceMssgPageIn(sender, input$messageFromAce)
      } 
      
      if( sender %in% c( 
          'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' , 'fileCmd.saveNow', 'buttonCmd.rmdViewer'
          )
      ){
        processMssgFromAceMssgPageOut(sender, input$messageFromAce) 
      }

      
    }
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE)



