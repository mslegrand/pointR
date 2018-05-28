

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
  #cat("cmdFileOpen:: Enter\n")
  if(getFileSavedStatus()==FALSE){
    #cat("getFileSavedStatus()==FALSE")
    showModal( modalSaveOrContinue() )
  } else {
    #cat("getFileSavedStatus()==TRUE\n")
    openFileDlgSelector()
  }
  
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
  # fullPath<-paste0(
  #   getCurrentDir(), getCurrentFile(), sep="/"
  # )
  #cat("reactive openFileDlgSelector:: sendCustomMessage\n")
  sendPtRManagerMessage(  sender='cmd.openFileNow', openFile=TRUE)
  # session$sendCustomMessage(
  #   type = "ptRManager", 
  #   list(id= getAceEditorId(), openFile=TRUE, sender='cmd.openFileNow' )
  # )
  # try(fileName<-dlgOpen(
  #   default=fullPath,
  #   title = "Select which R file to Open", 
  #   filters = dlgFilters[c("R", "All"), ])$res
  # )
  # openFileNow(fileName)
}


observeEvent(input$buttonFileOpenHidden,{
  #cat("observe input$buttonFileOpenHidden:: enter\n")
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonFileOpenHidden)
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
      # cat('openFileNow:: fileName=',fileName,"\n")
      addFileTab(title=tabName, txt=src, docFilePath= fileName)
      #here we set the value, 
      # session$sendCustomMessage(
      #   type = "shinyAceExt",
      #   list(id= getAceEditorId(), setValue=src, sender='cmd.openFileNow' )
      # )
      # cat("sendCustomMessage sender='cmd.file.open' aceId=",tabName,"\n" )
      delay(500,
            # session$sendCustomMessage(
            #   type = "shinyAceExt",
            #   list(id=  getAceEditorId(), sender='cmd.openFileNow', setValue= src, setDocFileSaved=TRUE, ok=TRUE)
            # )
            updateAceExt(id=  getAceEditorId(), sender='cmd.openFileNow', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
      )
      
    }
  }
  
 
}


