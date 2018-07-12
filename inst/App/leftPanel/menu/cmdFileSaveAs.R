# 0 either 
#    a. triggerRefresh rollback=false, sender is cmd.save
# or
#    b. commit
# 1 send message to ptRManager open save dialog
# 2 get the return, and set directory/name
# 3 write the file
cmdFileSaveAs<-function(){
  # sendPtRManagerMessage( id=tabId,  sender='cmd.saveFileAs', saveFile=TRUE, closing=!is.null(request$closeTab), type='R')
  
  setTabRequest(sender="fileCmd.saveAs", tabs=input$pages)
}
