observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar
  if(fileCmd=="New"){ #-----new
    txt<-codeTemplate
    user$code<-codeTemplate
    # the next  line update the ptRList; probably should redo with observer
    file$name<-"newSVG.R"
    selectedPoint$point.index<-0
    updateSelectInput(session, "ptRSelect",  choices=c("x"), selected="x" ) 
    updateNavbarPage(session, "editNavBar", selected ="Source") 
    updateNavbarPage(session, "plotNavBar", selected ="Points")
    updateNavbarPage(session, "tagFreq", selected ="Off") 
  }
  if(fileCmd=="Open"){ #-----open 
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
      }
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
    updateNavbarPage(session, "tagFreq", selected ="Off") 
  }
  if(fileCmd=="Save"){ #-----save
    fileName=""
    default="newfile.R"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(fileName!=""){ 
      file$name<-fileName
      txt<-user$code
      writeLines(txt, fileName)
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
  }
})

