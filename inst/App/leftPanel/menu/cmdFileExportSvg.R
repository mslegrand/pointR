cmdFileExportSvg<-function(){
  if(getFileNameStatus()==TRUE){
    svgFileName<-
      paste0(tools::file_path_sans_ext(getCurrentFile()),".svg")
  } else {
    svgFileName<-"unnamed.svg"
  }
  session$sendCustomMessage( #triggers click of buttonFileSaveHidden
    type = "ptRManager", 
    list(id= "source", exportSVG=TRUE, sender='cmd.exportSVGNow' )
  )  
}

observeEvent(input$buttonExportSVGHidden,{
  fp.dt<-parseSavePath(c(wd='~'), input$buttonExportSVGHidden)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    fileName<-as.character(fp.dt$datapath[1])
    src<- getCode()
    parsedCode<-parse(text=src)
    txt<-as.character(eval(parsedCode))
    writeLines(txt, fileName)
    if(getFileNameStatus()==FALSE){
      fileName<-sub("\\.svg$", ".R", fileName)
      setCurrentFilePath(fileName)
    }
  }
})

