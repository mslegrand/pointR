

cmdFileQuit<-reactive({
  fd<-getAllNamedUnsavedFiles()
  choices<-fd$filePath
  if(length(choices)>0){
    choices<-structure((fd$tabId), names=choices)
    showModal(fileQuitModal(choices))
  } else {
    cmdQuitNow()
  }
})

cmdQuitNow<-reactive({
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  js$closeWindow()
  Sys.sleep(1)
  stopApp()
})



fileQuitModal<-function(choices){
  # doQuit<-"shinyjs.triggerButtonOnEnter(event,\"quitNow\")"
  modalDialog( 
    # onkeypress=doQuit, 
    span('The following named files have unsaved changes.'), 
    div( class='ptR2',
         prettyCheckboxGroup(
           inputId = "namedUnsavedFilesChBox",
           label = "Check to Save", 
           choices = choices, 
           selected=NULL
         )
    ),
    footer = tagList(
      actionButton("checkAll", "Save All and Exit"),
      actionButton("quitNow",  "Save Selected and Exit")
    )
  ) 
} 



observeEvent(input$checkAll,{
  removeModal()
  saveDnippetsFileNames()
  savePage(input$pages)
  # now put tabs on tab list and save all
  # cat('>---> checkAll\n')
  selection<-getAllNamedUnsavedFiles()$tabId
  if(length(selection)==0){
    cmdQuitNow()
  } else {
    #iterate over each tab id selection and save each, then quit 
    setTabRequest(sender='fileCmd.quit', tabs=selection)
  }
  # cat('<---< checkAll\n')
})

observeEvent(input$quitNow,{
  selection<-input$namedUnsavedFilesChBox
  removeModal()
  saveDnippetsFileNames()
  savePage(input$pages)
  # now put tabs on tab list and save all
  if(length(selection)==0){
    cmdQuitNow()
  } else {
    #iterate over each tab id selection and save each, then quit 
    #print(paste(selection, collapse=", "))
    setTabRequest(sender='fileCmd.quit', tabs=selection)
  }
  
})



# observeEvent(input$modalSaveOrQuitCancel, {
#   removeModal()
# }) 
# 
# observeEvent(input$QuitWithoutSaving, {
#   js$closeWindow()
#   Sys.sleep(1)
#   stopApp()  
# }) 
# 
