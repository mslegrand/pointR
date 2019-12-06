cmdFileExportSvg<-function(){
  if(getFileNameStatus()==TRUE){
    svgFileName<-
      paste0(tools::file_path_sans_ext(getCurrentFile()),".svg")
  } else {
    svgFileName<-"unnamed.svg"
  }
  click('buttonSvgExport')
}

observeEvent(input$buttonSvgExport,{
  fp.dt<-parseSavePath(c(home='~'), input$buttonSvgExport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    fileName<-as.character(fp.dt$datapath[1])
    fileName<-gsub(pattern = '^NA/', "~/", fileName)
    src<- getCode()
    parsedCode<-parse(text=src)
    txt<-as.character(eval(parsedCode), new.env() )
    writeLines(txt, fileName)
    if(getFileNameStatus()==FALSE){ 
      fileName<-sub("\\.svg$", ".R", fileName) # appears to setCurrentFilePath unnamed.R ???
      setCurrentFilePath(fileName)
    }
  }
})

