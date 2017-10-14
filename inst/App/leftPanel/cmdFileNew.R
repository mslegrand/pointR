cmdFileNew<-function(){
  
  updateRightPanel("Points")
  src<-codeTemplate
  session$sendCustomMessage(
    type = "shinyAceExt",
    list(id= "source", sender='cmd.file.new', setValue= src, ok=TRUE)
  )

  selectedPoint$point.index<-0
  selectedPoint$name<-"x"
  reactiveTag$freq<-list()
  displayOptions$insertMode=TRUE
  displayOptions$showGrid=TRUE
  displayOptions$ptMode="Normal"
  mssg$error<-""
  updateSelectInput(session, "ptRSelect",  choices=c("x"), selected="x" ) 
  updateNavbarPage(session, "tagFreq", selected ="Off") 
  
}