cmdFileNew<-function(){
  # cat("cmdFileNew:\n")
  src<-codeTemplate
  
  
  tabName<-getNextAnonymousFileName()
  
  #aceId<-tabName2AceId(tabId)
  addFileTab(title=tabName, txt=codeTemplate,  docFilePath="?")
  delay(500, 
    updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= codeTemplate, setDocFileSaved=TRUE, ok=TRUE )
  )

  # session$sendCustomMessage(
  #   type = "shinyAceExt", 
  #   list(id= getAceEditorId(), setValue=src, sender='cmd.file.new' )
  # )
  mssg$error<-""

  
}


cmdFileNewRmd<-function(){
  src<-rmdTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdNewIOSlides<-function(){
  src<-ioslidesTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}