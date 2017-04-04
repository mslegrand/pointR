cmdFileNew<-function(){
  #updateNavbarPage(session, "plotNavBar", selected ="Points")
  updateRightPanel("Points")
  src<-codeTemplate
  # the next  line update the ptRList; probably should redo with observer
  #file$name<-"newSVG.R" #this will need to change

  setCode(src)
  isolate({
    editOption$.saved<-TRUE
  })
  selectedPoint$point.index<-0
  selectedPoint$name<-"x"
  reactiveTag$freq<-list()
  displayOptions$insertMode=TRUE
  displayOptions$showGrid=TRUE
  displayOptions$ptMode="Normal"
  mssg$error<-""
  updateSelectInput(session, "ptRSelect",  choices=c("x"), selected="x" ) 

  updateNavbarPage(session, "tagFreq", selected ="Off") 
  updateAceEditor( session,"source", value=getCode() ) 
}