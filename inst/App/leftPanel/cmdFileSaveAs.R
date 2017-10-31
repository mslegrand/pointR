# 0 either 
#    a. triggerRefresh rollback=false, sender is cmd.save
# or
#    b. commit
# 1 send message to ptRManager open save dialog
# 2 get the return, and set directory/name
# 3 write the file
cmdFileSaveAs<-function(){
  #cat('cmdFileSaveAs:: Entering\n')
  if(getFileNameStatus()==TRUE){
    default<-getCurrentFile()
  } else {
    default<-"Unnamed"
  }
  #cat('cmdFileSaveAs:: sendCustomMessage\n')
  session$sendCustomMessage( #triggers click of buttonFileSaveHidden
    type = "ptRManager", 
    list(id= "source", saveFile=TRUE, sender='cmd.saveFileNow' )
  )
}

# cmdFileSaveAs<-function(){
#   if(getFileNameStatus()==TRUE){
#     default<-getCurrentFile()
#   } else {
#     default<-"Unnamed"
#   }
#   #default="newfile.R"
#   #TODO alert if editOption$.unNamed==TRUE
#   
#   
# 
#   try({
#     fileName<-dlgSave(
#       default=default,
#       title = "Save Script to File", 
#       filters = dlgFilters[c("R", "All"), ]
#     )$res
#     
#     if(length(fileName)>0 && nchar(fileName)>0){
#       
#       txt<-getCode() 
#       writeLines(txt, fileName)
#       #editOption$currentFilePath<-fileName
#       setCurrentFilePath(fileName)
#       session$sendCustomMessage(
#         type = "shinyAceExt", 
#         list(id= "source", setClean=TRUE, sender='save', setOk=TRUE)
#       )
#       editOption$currentFile<-basename(fileName)
#       editOption$currentDirectory<-dirname(fileName) 
#     }
#   })
# }

observeEvent(input$buttonFileSaveHidden,{
  #cat('observe input$buttonFileSaveHidden:: entering\n')
  fp.dt<-parseSavePath(c(wd='~'), input$buttonFileSaveHidden)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    #cat('observe input$buttonFileSaveHidden:: triggeringRefresh\n')
    triggerRefresh(sender='cmd.saveFileNow', rollBack=FALSE, auxValue=datapath) #triggers refresh
    # then save source
  }
})

#saveFileNow(datapath)

# saveFileNow<-function(fileName){
#   if(length(fileName)>0 && nchar(fileName)>0){
#     #cat("fileName=",fileName,"\n")
#     #       txt<-getCode() 
#     #       writeLines(txt, fileName)
#     #       #editOption$currentFilePath<-fileName
#     #       setCurrentFilePath(fileName)
#     
#     #txt<-getCode() 
#     #writeLines(txt, fileName)
#   }
# }