

cmdFileQuit<-reactive({
  saveDnippetsFileNames()
  savePage(input$pages)
  cmdQuitNow()
})

cmdQuitNow<-reactive({
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  js$closeWindow()
  Sys.sleep(1)
  stopApp()
})


observeEvent(input$modalSaveOrQuitCancel, {
  removeModal()
}) 

observeEvent(input$QuitWithoutSaving, {
  js$closeWindow()
  Sys.sleep(1)
  stopApp()  
}) 

