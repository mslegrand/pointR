#--------DELETE COLUMN------------------------------

deleteColumnModal <- function(columnName = NULL) {
  warningMssg<-paste('Warning: About to delete the column:', columnName) 
  modalDialog(
    span('DELETING!!!'), 
    if(!is.null(columnName)){
      div(
        tags$b(warningMssg, style = "color: red;")
      ) 
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("deleteColumnButton", "Delete")
    )
  ) 
}



observeEvent(input$deleteColumnButton, {
  if(getTibEditState()==TRUE){
    indx<-getTibColumn()
    newPtDefs<-getPtDefs()
    newPtDefs$tib[[getAssetName()]]<-newPtDefs$tib[[getAssetName()]][,-indx]
    sender<-'deleteColumn'
    updateAceExtDef(newPtDefs, sender=sender)
    # update columnSelection
    removeModal() #close dialog
  }
})

