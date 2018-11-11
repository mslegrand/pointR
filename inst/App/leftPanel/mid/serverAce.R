
# observe({
#   data<-input$retryMssg2Ace
#   isolate({
#     cat('>---> oe:: retryMssg2Ace\n')
#     print(data)
#   })
#   if(!identical(data$errCnt,0)){
#       session$sendCustomMessage(
#         type = "shinyAceExt",
#         data
#       )
#       invalidateLater(300)
#   }
#   isolate({cat('<---< oe:: retryMssg2Ace\n')})
# })


# observeEvent(input$retryMssg2Ace,{
#   data<-input$retryMssg2Ace
#   cat('>---> input$retryMssg2Ace\n')
#   cat('data$id=', data$id,'\n')
#   cat('data$errCnt=',data$errCnt,'\n')
#   cat('data$sender=',data$sender,'\n')
#   cat("class(data)=",class(data),"\n")
#   n<-as.numeric(data$errCnt)
#   data$errCnt<-n-1
#   cat(paste(names(data), collapse=", "),"\n")
#   print(data)
#   Sys.sleep(.3)
#   # delay( 1000, {
#     cat('sending to shinyAceExt\n')
#     session$sendCustomMessage( type = "shinyAceExt", data )
#   #}) 
#   cat('<---< input$retryMssg2Ace\n')
# },priority = 85, ignoreNULL = TRUE, ignoreInit = TRUE, label='retryMssg2Ace')


observeEvent(input$messageFromAce, {
 cat("\n>----> messageFromAce", '*****       sender=',format(input$messageFromAce$sender),"\n")
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      aceId<-input$messageFromAce$id
      cat('aceId=',format(aceId),"\n")
      cat('sender=',format(sender),"\n")
      tabId<-aceID2TabID(aceId)
      clearErrorMssg()
      # browser()
      #if(!identical(tabId, getTibTabId() )) update selectedAsset from db
      stopifnot(!is.null(tabId))
      # cat('sender=',format(sender),"\n")
      if(length(input$messageFromAce$docFilePath)>0 ){
        
        docFilePath<-unlist(input$messageFromAce$docFilePath)
        cat('docFilePath=',docFilePath,"\n")
        cat('calling setFileDescPath(',tabId,", ",docFilePath,")\n")
        setFileDescPath(tabId, docFilePath)
        cat('calling savePage(',tabId,")\n")
        savePage(tabId)
      }
      if(length(input$messageFromAce$isSaved)>0){ 
        editOption$.saved <- input$messageFromAce$isSaved
        print(editOption$.saved)
        cat('calling setFileDescSaved(',tabId,", ", format(editOption$.saved), ")\n")
        setFileDescSaved(tabId, editOption$.saved)
        savePage(tabId)
      }
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code)  ){
          reqSelector<-input$messageFromAce$selector
          cat('calling setSelectedAssetFromAce(reqSelector)\n')
          setSelectedAssetFromAce(reqSelector)
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
   cat("<----< messageFromAce",'*****       sender=',format(input$messageFromAce$sender),"\n\n")
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE, label='messageFromAce')



