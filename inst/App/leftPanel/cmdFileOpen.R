

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
  if(getFileSavedStatus()==FALSE){
    showModal( modalSaveOrContinue() )
  } else {
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


openFileDlgSelector<-reactive({
  #mssg$error<-""
  #session$sendCustomMessage(type = "shinyAceExt", list(id= "source", ptRMode=TRUE))
  fullPath<-paste0(
    getCurrentDir(), getCurrentFile(), sep="/"
  )
  
  #fileName=""
  try(fileName<-dlgOpen(
    default=fullPath,
    title = "Select which R file to Open", 
    filters = dlgFilters[c("R", "All"), ])$res
  )
  openFileNow(fileName)
})

openFileNow<-function(fileName){
  if(length(fileName)>0 && nchar(fileName)>0){ 
    src<-paste(readLines(fileName), collapse = "\n")
    # editOption$currentFile<-basename(fileName)
    # editOption$currentDirectory<-dirname(fileName) 
    #editOption$currentFilePath<-filePath
    setCurrentFilePath(fileName)
    #setwd(editOption$currentDirectory) #TODO make this reactive
    setwd(dirname(fileName))
    #file$name<-fileName #TODO: reactive expr using editOption$currentFile
    if(nchar(src)>0){
      src<-preProcCode(src)
      user$code<-src
      isolate({
        editOption$.saved<-TRUE
      })
      reactiveTag$freq<-list()
      displayOptions$insertMode=TRUE
      displayOptions$showGrid=TRUE
      displayOptions$ptMode="Normal"
      mssg$error<-""
    }
  }
  #updateNavbarPage(session, "plotNavBar", selected ="Points")
  updateRightPanel("Points")
  updateNavbarPage(session, "tagFreq", selected ="Off") 
  
}


