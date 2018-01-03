cmdFileNew<-function(){
  # panels$state<-'point'  # !!! crude kludge!!! panels$right2 
  # cat("filenew\n")
  # panels$right2<-"point"
  # #updateRightPanel("tibEditor") #shows header (but not footer)
  # updateRightPanel("point") #shows mid (but not header)
 
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