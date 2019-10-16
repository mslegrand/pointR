
observeEvent(trigger$startup, {
    log.fin(startup)
    reOrgPanels(id=NULL, mode=NULL)
    readAuxDnippets()
    readAuxPreProcs()
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
