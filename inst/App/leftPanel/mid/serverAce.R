
observeEvent(input$messageFromAce, {
cat(">----> messageFromAce\n")
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      # cat('aaadf\n')
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      # cat('bbbb\n')
      aceId<-input$messageFromAce$id
      tabId<-aceID2TabID(aceId)
      # cat('cccc\n')
      clearErrorMssg()
      # cat('dddd\n')
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        setSelectedAssetFromAce(reqSelector)
      }
      # cat('ffff\n')
      if(length(input$messageFromAce$docFilePath)>0 ){
        docFilePath<-unlist(input$messageFromAce$docFilePath)
        setFileDescPath(tabId, docFilePath)
        savePage(tabId)
      }
      # cat('gggg\n')
      if(length(input$messageFromAce$isSaved)>0){ 
        editOption$.saved <- input$messageFromAce$isSaved
        setFileDescSaved(tabId, input$messageFromAce$isSaved)
        savePage(tabId)
      }
      
      # cat('hhhh\n')
      
      if(
        sender %in% c( 
          'cmd.file.new', 'cmd.tabChange', 'cmd.openFileNow', 
          'cmd.commit', 'cmd.add.column', 'cmd.add.asset' #, 'cmd.saveFileNow' 
          )
      ){#not sure if cmd.saveFileNow should be here, infact, cannot find sender issuing this.
        # cat('gggg\n')
        processMssgFromAceMssgPageIn(sender, input$messageFromAce)
      } 
      # cat('iiii\n')
      if( sender %in% c( 
          'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' , 'fileCmd.saveNow', 'buttonCmd.rmdViewer'
          )
      ){
        # cat('kkkk\n')
        processMssgFromAceMssgPageOut(sender, input$messageFromAce) 
      }
      # cat('sender=',format(input$messageFromAce$sender),'\n')
      # cat('llll\n')
    }
  cat("<----< messageFromAce\n")
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE, label='messageFromAce')



