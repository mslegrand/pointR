
observeEvent(trigger$startup, {
    log.fin(startup)
    rowGroupsDB(initialRowGroupDB())
    reOrgPanels(id=NULL, mode=NULL)
    readAuxDnippets()
    readAuxPreProcs()
    aceId<-restoreWorkSpace()
    if(is.null(aceId)){
        cmdFileNewPtR()
    } else {
      
    }
    # disableDMDM(
    #   session, 
    #   menuBarId="editNavBar", # can't find customControl
    #   entry="customControl"
    # )
    updateNewProjectMenu(session)
    updateRemoveTemplateMenu(session)
    
    log.fout(startup)
}, priority=100)
