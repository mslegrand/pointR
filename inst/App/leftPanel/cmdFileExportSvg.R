cmdFileExportSvg<-reactive({
  fileCmd<-input$editNavBar #This is necessary to trigger reactive expression
  

  default="unnamed.svg"
  curDir<-getCurrentDir()
  curFile<-getCurrentFile()
  if(getFileNameStatus()==TRUE){
    svgFileName<-
      paste0(tools::file_path_sans_ext(getCurrentFile()),".svg")
  } else {
    svgFileName<-"unnamed.svg"
  }
  default<-paste(curDir,svgFileName,sep="/")
  #print(default)
  try({
    fileName<-dlgSave(
      default=default,
      title = "Export SVG to File", 
      filters = dlgFilters[c("R", "All"), ]
    )$res
    # print(fileName)
    if(length(fileName)>0 && nchar(fileName)>0){
      #file$name<-fileName
      src<-user$code
      parsedCode<-parse(text=src)
      txt<-as.character(eval(parsedCode))
      writeLines(txt, fileName)
      if(getFileNameStatus()==FALSE){
        editOption$currentFile<-basename(fileName)
        editOption$currentDirectory<-dirname(fileName) 
      }
      
    }
  })
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  
  
})