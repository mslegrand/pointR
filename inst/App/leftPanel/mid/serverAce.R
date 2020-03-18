

observeEvent(input$messageFromAce, {
  # cat("\n>----> messageFromAce", '*****       sender=',format(input$messageFromAce$sender),"\n")
    if(length(input$messageFromAce$sender)>0){
      if(length(input$messageFromAce$code)>0){ # returning code  
        setCode(input$messageFromAce$code)  # only place where request$code is set
        sender<-input$messageFromAce$sender # ace returns sender from call to update
        aceId<-input$messageFromAce$id
        tabId<-aceID2TabID(aceId)
        clearErrorMssg()
        stopifnot(!is.null(tabId))
        # filePath
        if(length(input$messageFromAce$docFilePath)>0 ){
          docFilePath<-unlist(input$messageFromAce$docFilePath)
          setFileDescPath(tabId, docFilePath, pathToProj= editOption$currentProjectDirectory)
          savePage(tabId) #saves page to workspace
        }
        # save status
        if(length(input$messageFromAce$isSaved)>0){ #ace gets isSaved from the undomanager
          setFileDescSaved(tabId, input$messageFromAce$isSaved)
          savePage(tabId) #saves page to workspace
        }
        # reqSelector
        if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code)  ){
            reqSelector<-input$messageFromAce$selector
            setSelectedAssetFromAce(reqSelector)
        }
        
        # branch on sender
        if( sender %in% 
            c( 
              'cmd.tabChange', #'cmd.openFileNow', 'cmd.file.new', 
              'cmd.commit', 'cmd.add.column', 'cmd.add.asset' 
            ) # these all should redraw viewport
        ){
          processMssgFromAceMssgPageIn(sender, input$messageFromAce)
        } else if( sender %in% 
            c( 
            'fileCmd.save', 'fileCmd.saveAs' 
            )
        ){
          processMssgFromAceMssgPageOut(sender, input$messageFromAce) 
        } else {
          setTrigger('redraw')
        }
        # end of branch which returns code
      } else { # code not returned
        if(input$messageFromAce$sender=="saveStatusUpdate" &&
           length(input$messageFromAce$isSaved)>0){
              sender<-input$messageFromAce$sender
              aceId<-input$messageFromAce$id
              tabId<-aceID2TabID(aceId)
              setFileDescSaved(tabId, input$messageFromAce$isSaved)
              #savePage(tabId)
        }
      }
    }
   # cat("<----< messageFromAce",'*****       sender=',format(input$messageFromAce$sender),"\n\n")
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE, label='messageFromAce')



