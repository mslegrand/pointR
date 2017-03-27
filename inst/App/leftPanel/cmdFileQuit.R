

cmdFileQuit<-reactive({
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-unlist(opts)
  modalSaveOrQuit <- function(){
    modalDialog(
      span("File not saved, quit anyway?"), 
      footer = tagList(
        actionButton("modalSaveOrQuitCancel", "Cancel"),
        actionButton("QuitWithoutSaving", "Quit without saving") 
      )
    ) 
  }
  
  # TODO!!! ALERT if need to save, ie. editOption$.notSaved==TRUE
  status<-editOption$.saved
  if(getFileSavedStatus()==FALSE){
    showModal( modalSaveOrQuit() )
  } else {
    writeOptions(optionFile, opts)
    js$closeWindow()
    Sys.sleep(1)
    stopApp()
  }
})

observeEvent(input$modalSaveOrQuitCancel, {
  removeModal()
}) 

observeEvent(input$QuitWithoutSaving, {
  js$closeWindow()
  Sys.sleep(1)
  stopApp()  
}) 

