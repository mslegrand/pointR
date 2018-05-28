cmdFileNew<-function(){
  # cat("cmdFileNew:\n")
  src<-codeTemplate
  
  # session$sendCustomMessage(
  #   type = "shinyAceExt",
  #   list(id= getAceEditorId(), sender='cmd.file.new', setValue= src, ok=TRUE)
  # )
  
  tabName<-getNextAnonymousFileName()
  
  #aceId<-tabName2AceId(tabId)
  addFileTab(title=tabName, txt=codeTemplate,  docFilePath="?")
  #triggerRefresh('cmd.commit', rollBack=FALSE)
  #setSourceType(sourceType=svgPanelTag)
  #cat("sendCustomMessage sender='cmd.file.new' aceId=",aceId,"\n" )
  #updateSelected(columnName='x') #kludge for getting ensuring asset selection will be x
  delay(500, 
    # session$sendCustomMessage(
    #   type = "shinyAceExt",
    #   list(id=  getAceEditorId(), sender='cmd.file.new', setValue= codeTemplate, setDocFileSaved=TRUE, ok=TRUE)
    # ) 
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
  
  # session$sendCustomMessage(
  #   type = "shinyAceExt",
  #   list(id= getAceEditorId(), sender='cmd.file.new', setValue= src, ok=TRUE)
  # )
  
  tabName<-getNextAnonymousFileName()
  
  #aceId<-tabName2AceId(tabId)
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  #triggerRefresh('cmd.commit', rollBack=FALSE)
  #setSourceType(sourceType=svgPanelTag)
  #cat("sendCustomMessage sender='cmd.file.new' aceId=",aceId,"\n" )
  #updateSelected(columnName='x') #kludge for getting ensuring asset selection will be x
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
        # session$sendCustomMessage(
        #   type = "shinyAceExt",
        #   list(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE)
        # ) 
  )
  
  # session$sendCustomMessage(
  #   type = "shinyAceExt", 
  #   list(id= getAceEditorId(), setValue=src, sender='cmd.file.new' )
  # )
  mssg$error<-""
  
}