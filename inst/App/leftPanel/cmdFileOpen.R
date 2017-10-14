

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
  fullPath<-paste0(
    getCurrentDir(), getCurrentFile(), sep="/"
  )
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
    setCurrentFilePath(fileName)
    setwd(dirname(fileName))
    if(nchar(src)>0){
      reactiveTag$freq<-list()
      displayOptions$insertMode=TRUE
      displayOptions$showGrid=TRUE
      displayOptions$ptMode="Normal"
      mssg$error<-""
      session$sendCustomMessage(
        type = "shinyAceExt", 
        list(id= "source", setValue=src, sender='openFileNow' )
      )
      
    }
  }
  updateRightPanel("Points")
  updateNavbarPage(session, "tagFreq", selected ="Off") 
  
}


