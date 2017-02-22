cmdFileExportSvg<-function(){
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
  
  try({
    fileName<-dlgSave(
      default=default,
      title = "Export SVG to File", 
      filters = dlgFilters[c("R", "All"), ]
    )$res
    
    if(length(fileName)>0 && nchar(fileName)>0){
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
}