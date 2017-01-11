cmdFileNew<-reactive({
  fileCmd<-input$editNavBar #This is necessary to trigger reactive expression
  
  updateNavbarPage(session, "plotNavBar", selected ="Points")
  src<-codeTemplate
  # the next  line update the ptRList; probably should redo with observer
  #file$name<-"newSVG.R" #this will need to change
  user$code<-src
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
  updateNavbarPage(session, "editNavBar", selected ="tab1") 
  
  updateNavbarPage(session, "tagFreq", selected ="Off") 
  #session$sendCustomMessage(type = "shinyAceExt", list(id= "source", ptRMode=TRUE))
  updateAceEditor( session,"source", value=user$code) 
})