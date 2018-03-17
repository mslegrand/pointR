cmdFileNew<-function(){
  
  src<-codeTemplate
  
  session$sendCustomMessage(
    type = "shinyAceExt",
    list(id= getAceEditorId(), sender='cmd.file.new', setValue= src, ok=TRUE)
  )

  mssg$error<-""
  
}