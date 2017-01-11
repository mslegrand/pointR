

cmdFileQuit<-reactive({
  fileCmd<-input$editNavBar #This is necessary to trigger reactive expression

  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-unlist(opts)
  
  modalSaveOrQuit <- function() {
    modalDialog(
      span("File not saved, quit anyway?"), 
      footer = tagList(
        actionButton("modalSaveOrQuitCancel", "Cancel"),
        actionButton("QuitWithoutSaving", "Quit without saving") 
      )
    ) 
  }
  
  # TODO!!! ALERT if need to save, ie. editOption$.notSaved==TRUE
  if(getFileSavedStatus()==FALSE){
    showModal( modalSaveOrQuit() )
  } else {
    writeOptions(optionFile, opts)
    js$closeWindow()
    stopApp()
  }
})

observeEvent(input$modalSaveOrQuitCancel, {
  removeModal()
}) 

observeEvent(input$QuitWithoutSaving, {
  writeOptions(optionFile, opts)
  js$closeWindow()
  stopApp()  
  removeModal()
}) 

