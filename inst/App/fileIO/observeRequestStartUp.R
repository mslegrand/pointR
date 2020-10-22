
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
    
    updateNewProjectMenu(session)
    updateRemoveTemplateMenu(session)
    resetWatcher()
    log.fout(startup)
}, priority=100)


resetWatcher<-reactive({
  if(usingElectron){
    allFilePaths<-getAllNamedFiles()$filePath
    allFilePaths<-normalizePath(allFilePaths)
    # cat('pointR::resetWatcher: allFilePaths')
    # print(allFilePaths)
    sendPtRManagerMessage(sender="cmd.electron", resetWatcher=allFilePaths)
  }
})