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
  if(rightPanel()=='tibEditor'){
    indx<-getTibColumn()
    newPtDefs<-getPtDefs()
    newPtDefs$tib[[getTibName()]]<-newPtDefs$tib[[getTibName()]][,-indx]
    sender<-'deleteColumn'
    updateAceExtDef(newPtDefs, sender=sender)
    # update columnSelection
    removeModal() #close dialog
  }
})

