

modalSaveOrContinue <- function() {
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
  fp.dt<-parseFilePaths(c(home='~'), input$buttonFileOpen)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    openFileNow(datapath)
  }
})

openFileNow<-function(fileName){
  if(length(fileName)>0 && nchar(fileName)>0){ 
    src<-paste(readLines(fileName), collapse = "\n") #this assumes absolute path unless we have getwd()==projDir
    removeFromRecentFiles(fileName)
    setCurrentFilePath(fileName) # should this be replaced by shinyFiles???
    setwd(dirname(fileName))  # should this be replaced by shinyFiles???
    if(nchar(src)>0){
      mssg$error<-""
      tabName<-basename(fileName)
      fileExt<-tail(splitByPattern(fileName, '\\.'),1)
      mode<-pathExt2mode(fileExt)
      addFileTab(title=tabName, txt=src, docFilePath= fileName, mode=mode, fileSaveStatus=TRUE)
      
    }
  }
  
 
}


