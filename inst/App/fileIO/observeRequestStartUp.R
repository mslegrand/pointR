observeEvent(request$sender,{
  if(identical(request$sender,'startup')){ 
    #cat(">---> request startup\n")
    # cat('readDnippetsFileNames\n')
    readDnippetsFileNames()
    # cat('now to saveDnippetsFileNames\n')
    saveDnippetsFileNames()
    # cat('now to restoreWorkSpace\n')
    success<-restoreWorkSpace()
    # cat('success=',format(success),"\n")
    if(!success){
      # cat('now to cmdFileNewPtR\n')
      cmdFileNewPtR()
    } 
    # cat('now to disableDMD\n')
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="customControl"
    )
    resetShinyFilesIOPaths(getDirPath()) #lets just force this to happen
    #dirtyDMDM(session, "editNavBar")
    # cat("<---< request startup\n")
  }
}, priority=100)
