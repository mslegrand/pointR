cmdFileSave<-function(){
  # curDir<-getCurrentDir()
  # curFile<-getCurrentFile()
  # if(nchar(curDir)>0 && 
  #    nchar(curFile)>0 
  # )
  if(getFileNameStatus()==TRUE){
    #fullFilePath<-paste(curDir, curFile, sep="/")
    fullFilePath<-getCurrentFilePath()
    isolate({
      editOption$.saved<-TRUE
    })
    txt<-getCode() 
    writeLines(txt, fullFilePath)
  }
}