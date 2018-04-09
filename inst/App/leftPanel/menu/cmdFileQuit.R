

cmdFileQuit<-reactive({
  cat('inside cmdFileQuit')
  session$sendCustomMessage(
    type = "scrollManager", 
    list( sender= 'fileCmd.quit', getAllTabIds=runif(1) ) 
  )
})

cmdQuitNow<-reactive({
  cat('inside cmdQuitNow')
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  js$closeWindow()
  Sys.sleep(1)
  stopApp()
})


# cmdFileQuit<-reactive({
#   opts<-isolate(reactiveValuesToList((editOption)))
#   modalSaveOrQuit <- function(){
#     modalDialog(
#       span("File not saved, quit anyway?"), 
#       footer = tagList(
#         actionButton("modalSaveOrQuitCancel", "Cancel"),
#         actionButton("QuitWithoutSaving", "Quit without saving") 
#       )
#     ) 
#   }
#   
#   if(getFileSavedStatus()==FALSE){
#     showModal( modalSaveOrQuit() )
#   } else {
#     opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
#     writeOptionsJSON(opts)
#     js$closeWindow()
#     Sys.sleep(1)
#     stopApp()
#   }
# })
# 
observeEvent(input$modalSaveOrQuitCancel, {
  removeModal()
}) 

observeEvent(input$QuitWithoutSaving, {
  js$closeWindow()
  Sys.sleep(1)
  stopApp()  
}) 

