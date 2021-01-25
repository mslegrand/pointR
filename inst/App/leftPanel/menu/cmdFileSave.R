cmdFileSave<-function(){
  setTabRequest(cmd="fileCmd.save", tabs=input$pages)
}

cmdFileSaveAll<-function(){
  tabIds<-getAllNamedUnsavedFiles()$tabId
  if(length(tabIds)>0){
    setTabRequest(cmd= 'fileCmd.save', tabs=tabIds)
  }
}

observeEvent(getFileDescriptor(input$pages),{
 
  fd<-getFileDescriptor(input$pages)
  if(nrow(fd)>0){
    if( fd$isSaved==FALSE  && fd$filePath!="?"){
      enableDMDM(session,  menuBarId="editNavBar", entry="Save")
    }else{
      disableDMDM(session,  menuBarId="editNavBar", entry="Save")
    } 
  }
})


observeEvent(getAllNamedUnsavedFiles(),{
  fd<-getAllNamedUnsavedFiles()
  if(nrow(fd)==0){
    disableDMDM(session,  menuBarId="editNavBar", entry="saveAll")
  } else {
    enableDMDM(session,  menuBarId="editNavBar", entry="saveAll")
  }
})

  