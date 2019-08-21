

observeEvent(input$messageFromAce, {
 # cat("\n>----> messageFromAce", '*****       sender=',format(input$messageFromAce$sender),"\n")
    if(length(input$messageFromAce$sender)>0){
      if(length(input$messageFromAce$code)>0){
        request$code<-input$messageFromAce$code # only place where request$code is set
        sender<-input$messageFromAce$sender # ace returns sender from call to update
        #request$sender<-sender 
        setRequestSender(sender)
        aceId<-input$messageFromAce$id
        tabId<-aceID2TabID(aceId)
        clearErrorMssg()
        # browser()
        #if(!identical(tabId, getTibTabId() )) update selectedAsset from db
        stopifnot(!is.null(tabId))
        # cat('sender=',format(sender),"\n")
        if(length(input$messageFromAce$docFilePath)>0 ){
          
          docFilePath<-unlist(input$messageFromAce$docFilePath)
          # cat('docFilePath=',docFilePath,"\n")
          # cat('calling setFileDescPath(',tabId,", ",docFilePath,")\n")
          setFileDescPath(tabId, docFilePath, pathToProj= editOption$currentProjectDirectory)
          # cat('calling savePage(',tabId,")\n")
          savePage(tabId)
        }
        if(length(input$messageFromAce$isSaved)>0){ #ace gets isSaved from the undomanager
          #editOption$.saved <- input$messageFromAce$isSaved
          # print(editOption$.saved)
          # cat('calling setFileDescSaved(',tabId,", ", format(editOption$.saved), ")\n")
          #setFileDescSaved(tabId, editOption$.saved)
          setFileDescSaved(tabId, input$messageFromAce$isSaved)
          savePage(tabId) #saves page to workspace
        }
        if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code)  ){
            reqSelector<-input$messageFromAce$selector
            setSelectedAssetFromAce(reqSelector)
        }
        if(
          sender %in% c( 
            'cmd.tabChange', #'cmd.openFileNow', 'cmd.file.new', 
            'cmd.commit', 'cmd.add.column', 'cmd.add.asset' 
            ) # these all should redraw viewport
        ){
          
          processMssgFromAceMssgPageIn(sender, input$messageFromAce)
        } else if( 
          sender %in% c( 
            'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' , 'fileCmd.saveNow', 'buttonCmd.rmdViewer'
            )
        ){
          processMssgFromAceMssgPageOut(sender, input$messageFromAce) 
        } else {
          setTrigger('redraw')
        }
      
      } else {
        if(input$messageFromAce$sender=="saveStatusUpdate" &&
           length(input$messageFromAce$isSaved)>0){
          sender<-input$messageFromAce$sender
          aceId<-input$messageFromAce$id
          cat('aceId=',format(aceId),"\n")
          cat('sender=',format(sender),"\n")
          cat('isSaved=',input$messageFromAce$isSaved,"\n")
          tabId<-aceID2TabID(aceId)
           setFileDescSaved(tabId, input$messageFromAce$isSaved)
           #savePage(tabId)
        }
      }
    }
   # cat("<----< messageFromAce",'*****       sender=',format(input$messageFromAce$sender),"\n\n")
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE, label='messageFromAce')



