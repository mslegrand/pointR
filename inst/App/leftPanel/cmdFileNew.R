cmdFileNew<-function(){
  
  updateRightPanel("tibEditor")
  src<-codeTemplate
  session$sendCustomMessage(
    type = "shinyAceExt",
    list(id= "source", sender='cmd.file.new', setValue= src, ok=TRUE)
  )

  
  reactiveTag$freq<-list()
  
  # displayOptions$insertMode=TRUE
  # displayOptions$showGrid=FALSE
  # displayOptions$ptMode="Normal"
  mssg$error<-""
  #updateSelectInput(session, "ptRSelect",  choices=c("x"), selected="x" ) # !!! DOES NOTHING
  #updateNavbarPage(session, "tagFreq", selected ="Off") # !!! DOES NOTHING
  
}