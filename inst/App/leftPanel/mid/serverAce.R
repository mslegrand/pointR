
observeEvent(input$messageFromAce, {
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      clearErrorMssg()
      
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      
      if(length(input$messageFromAce$isSaved)>0){ 
        aceId<-input$messageFromAce$id
        editOption$.saved <- input$messageFromAce$isSaved
      }
      

      if(
        sender %in% c( 
          'cmd.file.new', 'cmd.tabChange', 'cmd.openFileNow', 
          'cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow' 
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



updateSelected4Ace<-function( reqSelector){
  # updatEle<- c('name', 'ptColName', 'rowIndex', 'matCol', 'colName')
  cat('names of reqSelector:', paste(names(reqSelector), collapse=", "),"\n" )
  for(n in names(reqSelector)){
    stopifnot({n %in% names(selectedAsset)})
    selectedAsset[[n]]<-reqSelector[[n]]
  }
}

