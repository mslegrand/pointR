
genShinySaveFilesServerConnection<-function(input, session){
  lapply(saveButtonFileNames, function(n){
    shinyFileSave(input, n,   session=session, roots=c(wd="~") )
  })
}

#needs to be in server
genShinySaveFilesObservers<-function(input, session){
  lapply(saveButtonFileNames, function(n){
    observeEvent(input[[n]], {
      rtList<-input[[n]]
      if('cancel' %in% names(rtList)){
        if(rtList$cancel=='close'){ 
          # cat('cancel and close')
          tabId<-popTab()
          closeTabNow(tabId)
        } else {
          setTabRequest(sender=NULL, tabs=NULL)
        }
      } else { 
        cat('parseSavePath 123\n')
        fp.dt<-parseSavePath(c(wd='~'), rtList)
        if(length(fp.dt)>0 && nrow(fp.dt)>0){
          cat('=======shinyFiles SAVE observer=============\n')
          
          datapath<-as.character(fp.dt$datapath[1])
          # 
          
          # TODO!!!  add oldPath to recentFiles (this assumes that we can do a saveAs)
          #  The safest time is after the file has been saved under the new path, but that
          #  means the old filePath needs to be kept just a little longer ...
          #
          # We just got a new path name  So ...
          # 
          # 1. if sender is fileCmd.saveAs, change to sender = 'fileCmd.save' since we don't need to get a new path anymore
          # 2. send to aceExt.js new path  (sender will be either fileCmd.save or fileCmd.close)
          # 3. aceExt.js returns to serverAce.R with 
          #      sender either fileCmd.save or fileCmd.close
          #      the new docFilePath 
          #      saved=FALSE
          #      code
          # 4. serverAce.R 
          #      saves to docFilePath
          #      sets doc as SAVED
          #      If sender==close, removeTab
          #      ow. update tab title
          sender=request$sender
          if(request$sender=='fileCmd.saveAs'){
            sender='fileCmd.saveNow'
          }
          cat('sender=',format(sender),"\n")
          tabId<-peekTab()
          aceId<-tabID2aceID(tabId)
          
          # Now the sender can be close, save or saveAs, but we leave this to ace, then we need a flag to say that we changed the name!
          updateAceExt( id=aceId, setDocFilePath=datapath,  sender=sender, getDoc=TRUE)
        }
      }
    })
  })
}

genShinySaveFilesServerConnection(input, session)
genShinySaveFilesObservers(input, session)
