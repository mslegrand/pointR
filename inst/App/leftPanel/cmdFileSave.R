
cmdFileSave<-reactive({
  fileCmd<-input$editNavBar #This is necessary to trigger reactive expression
  curDir<-getCurrentDir()
  curFile<-getCurrentFile()
  if(nchar(curDir)>0 && 
     nchar(curFile)>0 
  ){
    fullFilePath<-paste(curDir, curFile, sep="/")
    isolate({
      editOption$.saved<-TRUE
    })
    txt<-user$code
    writeLines(txt, fullFilePath)
  }
  updateNavbarPage(session, "editNavBar", selected ="tab1")
})