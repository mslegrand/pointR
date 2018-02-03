# 0 either 
#    a. triggerRefresh rollback=false, sender is cmd.save
# or
#    b. commit
# 1 send message to ptRManager open save dialog
# 2 get the return, and set directory/name
# 3 write the file
cmdFileSaveAs<-function(){
  if(getFileNameStatus()==TRUE){
    default<-getCurrentFile()
  } else {
    default<-"Unnamed"
  }
  session$sendCustomMessage( #triggers click of buttonFileSaveHidden
    type = "ptRManager", 
    list(id= "source", saveFile=TRUE, sender='cmd.saveFileNow' )
  )
}

observeEvent(input$buttonFileSaveHidden,{
  fp.dt<-parseSavePath(c(wd='~'), input$buttonFileSaveHidden)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    triggerRefresh(sender='cmd.saveFileNow', rollBack=FALSE, auxValue=datapath) #triggers refresh
    # then save source
  }
})

