#observeEvent(request$sender,{
  #if(identical(request$sender,'startup')){ 
observeEvent(trigger$startup, {
    log.fin(startup)
    reOrgPanels(id=NULL, mode=NULL)
    readDnippetsFileNames()
    saveDnippetsFileNames()
    aceId<-restoreWorkSpace()
    if(is.null(aceId)){
        cmdFileNewPtR()
    } else {
      
    }
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="customControl"
    )
    
    log.fout(startup)
  # }
}, priority=100)
