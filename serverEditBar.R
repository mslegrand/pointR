observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar
  if(fileCmd=="New"){ #-----new
    updateNavbarPage(session, "plotNavBar", selected ="Points")
    src<-codeTemplate
    # the next  line update the ptRList; probably should redo with observer
    file$name<-"newSVG.R"
    user$code<-src
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
  }
  
  if(fileCmd=="Open"){ #-----open 
    #mssg$error<-""
    #session$sendCustomMessage(type = "shinyAceExt", list(id= "source", ptRMode=TRUE))
    fileName=""
    try(fileName<-dlgOpen(title = "Select one R file", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(length(fileName)>0 && nchar(fileName)>0){ 
      src<-paste(readLines(fileName), collapse = "\n")
      file$name<-fileName
      if(nchar(src)>0){
        src<-preProcCode(src)
        user$code<-src
        reactiveTag$freq<-list()
        displayOptions$insertMode=TRUE
        displayOptions$showGrid=TRUE
        displayOptions$ptMode="Normal"
        mssg$error<-""
      }
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
    updateNavbarPage(session, "tagFreq", selected ="Off") 
  }
  if(fileCmd=="Save"){ #-----save
    fileName=""
    default="newfile.R"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(length(fileName)>0 && fileName!=""){ 
      file$name<-fileName
      txt<-user$code
      writeLines(txt, fileName)
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
  }
  if(fileCmd=="Export as SVG"){ #-----save
    fileName=""
    default="newfile.svg"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(fileName!=""){ 
      file$name<-fileName
      src<-user$code
      parsedCode<-parse(text=src)
      txt<-as.character(eval(parsedCode))
      writeLines(txt, fileName)
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
  }
  
})

#------fileName-------------
output$fileName <- renderText({ 
  fileName<-file$name
  if(is.null(fileName) ){
    fileName==""
  }
  paste("Editing", basename(fileName))
})

# -----------ACE EDITOR------------------------
observeEvent(
  user$code, {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=user$code)
    }
  }
)

