

cmdFileQuit<-reactive({
  opts<-isolate(reactiveValuesToList((editOption)))
  modalSaveOrQuit <- function(){
    modalDialog(
      span("File not saved, quit anyway?"), 
      footer = tagList(
        actionButton("modalSaveOrQuitCancel", "Cancel"),
        actionButton("QuitWithoutSaving", "Quit without saving") 
      )
    ) 
  }
  
  status<-editOption$.saved
  if(getFileSavedStatus()==FALSE){
    showModal( modalSaveOrQuit() )
  } else {
    writeOptionsJSON(opts)
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

