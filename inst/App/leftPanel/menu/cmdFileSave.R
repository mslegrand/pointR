cmdFileSave<-function(){
  setTabRequest(sender="fileCmd.save", tabs=input$pages)
}

cmdFileSave<-function(){
  sendFileTabsMessage(sender= 'fileCmd.saveNow', getAllTabIds=runif(1))
}


  