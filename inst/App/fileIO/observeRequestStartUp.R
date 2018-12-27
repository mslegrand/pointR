observeEvent(request$sender,{
  if(identical(request$sender,'startup')){ 
    
    # cat_list<<-c( cat_list,">---> request startup\n")
    # cat('reOrgPanels\n')
    reOrgPanels(id=NULL, mode=NULL)
    
     # cat('readDnippetsFileNames\n')
    readDnippetsFileNames()
     # cat('now to saveDnippetsFileNames\n')
    saveDnippetsFileNames()
     # cat('now to restoreWorkSpace\n')
    aceId<-restoreWorkSpace()
     # cat('aceId=',format(aceId),"\n")
    if(is.null(aceId)){
      # cat('now to cmdFileNewPtR\n')
      cmdFileNewPtR()
    } else {
      #updateAceExt(id=aceId, sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
    }
    # cat('now to disableDMD\n')
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="customControl"
    )
    # updateAceExt(id=aceId, sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
    # resetShinyFilesIOPaths(getDirPath()) #lets just force this to happen
    #dirtyDMDM(session, "editNavBar")
    # cat_list<<-c( cat_list,"<---< request startup\n\n")
  }
}, priority=100)
