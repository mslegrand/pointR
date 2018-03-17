cmdFileSave<-function(){
  
  if(getFileNameStatus()==TRUE){
    
    fullFilePath<-getCurrentFilePath()
      # editOption$.saved<-TRUE
    txt<-getCode() 
    writeLines(txt, fullFilePath)
    session$sendCustomMessage(
      type = "shinyAceExt", 
      list(id= getAceEditorId(), setClean=TRUE, sender='save', setOk=TRUE)
    )
  }
}

