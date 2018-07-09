cmdFileExportSvg<-function(){
  if(getFileNameStatus()==TRUE){
    svgFileName<-
      paste0(tools::file_path_sans_ext(getCurrentFile()),".svg")
  } else {
    svgFileName<-"unnamed.svg"
  }
  click('buttonExportSVG')
}

observeEvent(input$buttonExportSVG,{
  # cat('parseSavePath asdfasf\n')
  fp.dt<-parseSavePath(c(wd='~'), input$buttonExportSVG)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    fileName<-as.character(fp.dt$datapath[1])
    src<- getCode()
    # cat("buttonExportSVG::parse\n")
    parsedCode<-parse(text=src)
    txt<-as.character(eval(parsedCode))
    writeLines(txt, fileName)
    if(getFileNameStatus()==FALSE){
      fileName<-sub("\\.svg$", ".R", fileName)
      setCurrentFilePath(fileName)
    }
  }
})

