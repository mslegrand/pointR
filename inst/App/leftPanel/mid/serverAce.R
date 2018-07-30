
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
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        setSelectedAssetFromAce(reqSelector)
      }
      if(length(input$messageFromAce$docFilePath)>0 ){
        docFilePath<-unlist(input$messageFromAce$docFilePath)
        setFileDescPath(tabId, docFilePath)
        savePage(tabId)
      }
      if(length(input$messageFromAce$isSaved)>0){ 
        editOption$.saved <- input$messageFromAce$isSaved
        setFileDescSaved(tabId, input$messageFromAce$isSaved)
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



