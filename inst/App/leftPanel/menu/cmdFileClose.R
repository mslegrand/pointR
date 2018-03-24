cmdFileClose<-function(){
  tabId<-input$pages
  closeFile(tabId)
}

closeFile<-function(tabId){
  request$closeTab=tabId
  aceId<-tabID2aceID(tabId)
  updateAceExt( id=aceId, sender='fileCmd.save', getDoc=TRUE)
}

