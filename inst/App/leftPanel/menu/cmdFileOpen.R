

modalSaveOrContinue <- function() {
  #cat("modalSaveOrContinue:: Enter\n")
  modalDialog(
    span("File has unsaved changes. Continue or Save?"), 
    footer = tagList(
      actionButton("saveFirst", "Save First"),
      actionButton("continueOpen", "Continue") 
    )
  ) 
}

cmdFileOpen<-function(){
    openFileDlgSelector()
}

observeEvent(input$saveFirst, {
  removeModal()
  cmdFileSaveAs()
  #to do openFileDlgSelector requires currentDir!!!
  openFileDlgSelector()
}) 

observeEvent(input$continueOpen, {
  removeModal()
  openFileDlgSelector()
}) 

#  openFileDlgSelector changes  
# 1. send click message to openFile
# 2. create reactive to monitor any name/path change of the input$hiddenFileOpen button
#   2.1. if change, 
#          a. load file 
#          b. update framework
#          c. update Ace via openFileNow (or maybe like a request$sender=startup (or new), but with a different src)
openFileDlgSelector<-function(){
  sendPtRManagerMessage(  sender='cmd.openFileNow', openFile=TRUE)
}

observeEvent(input$buttonFileOpen,{
  # cat("observe input$buttonFileOpen:: enter\n")
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonFileOpen)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    openFileNow(datapath)
  }
})

openFileNow<-function(fileName){
  # cat("openFileNow:: enter\n")
  if(length(fileName)>0 && nchar(fileName)>0){ 
    src<-paste(readLines(fileName), collapse = "\n")
    removeFromRecentFiles(fileName)
    setCurrentFilePath(fileName) # should this be replaced by shinyFiles???
    setwd(dirname(fileName))  # should this be replaced by shinyFiles???
    if(nchar(src)>0){
      mssg$error<-""
      tabName<-basename(fileName)
      fileExt<-tail(splitByPattern(fileName, '\\.'),1)
      mode<-pathExt2mode(fileExt)
      #cat('openFileNow:: fileName=', format(fileName),"\n")
      #mode<-'txt'
      # if( grepl("*.ptr$", fileName, ignore.case = T)){
      #   mode<-'ptr'
      # }
      # if( grepl("*.r$", fileName, ignore.case = T)){
      #   mode<-'ptr'
      # }
      # if( grepl("*.Rmd$", fileName, ignore.case = T)){
      #   mode<-'ptrrmd'
      # }
      # if( grepl("*.snippets$", fileName, ignore.case = T)){
      #   mode<-'snippets'
      # }
      # if( grepl("*.dnippets$", fileName, ignore.case = T)){
      #   mode<-'dnippets'
      # }
      #if ptr mode, try to parse, if not parsable, get Error, set choices=R, error, and do error report????
    
      addFileTab(title=tabName, txt=src, docFilePath= fileName, mode=mode, TRUE)
      #here we get the code and set the doc status as saved, 
      delay(500,
            updateAceExt(id=getAceEditorId(), sender='cmd.openFileNow', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
      )
      
    }
  }
  
 
}


