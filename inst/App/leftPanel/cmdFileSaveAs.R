
cmdFileSaveAs<-function(){
  if(getFileNameStatus()==TRUE){
    default<-getCurrentFile()
  } else {
    default<-"Unnamed"
  }
  #default="newfile.R"
  #TODO alert if editOption$.unNamed==TRUE
  try({
    
  
    fileName<-dlgSave(
      default=default,
      title = "Save Script to File", 
      filters = dlgFilters[c("R", "All"), ]
    )$res
    
    if(length(fileName)>0 && nchar(fileName)>0){
      isolate({
        editOption$.saved<-TRUE
      })
      txt<-user$code
      writeLines(txt, fileName)
      editOption$currentFile<-basename(fileName)
      editOption$currentDirectory<-dirname(fileName) 
    }
  })
}