observeEvent(request$sender,{
  if(identical(request$sender,'startup')){ 
    log.fin(startup)
    reOrgPanels(id=NULL, mode=NULL)
    readDnippetsFileNames()
    saveDnippetsFileNames()
    aceId<-restoreWorkSpace()
    if(is.null(aceId)){
        cmdFileNewPtR()
    } else {
      # updateAceExt(id=aceId, sender='cmd.file.new', getValue= TRUE,  ok=TRUE ) #??
    }
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="customControl"
    )
    # updateAceExt(id=aceId, sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
    # resetShinyFilesIOPaths(getDirPath()) #lets just force this to happen
    # dirtyDMDM(session, "editNavBar")
    log.fout(startup)
  }
}, priority=100)
