cmdFileSave<-function(){
  
  if(getFileNameStatus()==TRUE){
    
    fullFilePath<-getCurrentFilePath()
    isolate({
      editOption$.saved<-TRUE
    })
    txt<-getCode() 
    writeLines(txt, fullFilePath)
  }
}