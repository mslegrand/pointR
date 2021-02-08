

cmdFileQuit<-reactive({
  log.fin(cmdFileQuit)
  storeAssetState() # should check if 
  if("parId" %in% names( fileDescDB())){ #should remove all child tabs
    aids<-filter(fileDescDB(), !is.na(parId) & filePath=="?")$tabId
    if(length(aids)>0){
      tabs<-aceID2TabID(aids)
      closeTabsNow(tabs)
    }
  }
  fd<-getAllNamedUnsavedFiles()
  choices<-fd$filePath
  if(length(choices)>0){
    choices<-structure((fd$tabId), names=choices)
    showModal(fileQuitModal(choices))
  } else {
    cmdQuitNow()
  }
  log.fout(cmdFileQuit)
})

cmdQuitNow<-reactive({
  savePage(input$pages)
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  sendPtRManagerMessage(sender='closePtRWindowNow', now=TRUE)
  Sys.sleep(1)
  
  stopApp()
})



fileQuitModal<-function(choices){
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
  savePage(input$pages)
  # now put tabs on tab list and save all
  selection<-getAllNamedUnsavedFiles()$tabId
  if(length(selection)==0){
    cmdQuitNow()
  } else {
    #iterate over each tab id selection and save each, then quit 
    setTabRequest(cmd='fileCmd.quit', tabs=selection)
  }
})

observeEvent(input$quitNow,{
  selection<-input$namedUnsavedFilesChBox
  removeModal()
  savePage(input$pages)
  # now put tabs on tab list and save all
  if(length(selection)==0){
    cmdQuitNow()
  } else {
    #iterate over each tab id selection and save each, then quit 
    setTabRequest(cmd='fileCmd.quit', tabs=selection)
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
