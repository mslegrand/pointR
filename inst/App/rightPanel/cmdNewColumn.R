

#--------NEW COLUMN------------------------------
addNewColModal <- function(errMssg=NULL) {
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  modalDialog(
    onkeypress=doOk, 
    span('Enter both a name for the new column and a value for its entries'), 
    textInput("modalAttrName", "Enter the name for the new column"),
    textInput("modalAttrValue", "Enter an entry value for the new column"), 
    if(!is.null(errMssg)){
      div(tags$b(errMssg, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("commitNewCol", "Commit")
    )
  ) 
}


observeEvent(input$commitNewCol, {
  #if(identical( rightPanel(), 'tibEditor')){
    #checks 
    if(!grepl(pattern = "^[[:alpha:]]", input$modalAttrName)){ # check name syntax
      showModal(attrValueModal( errMssg="Invalid Column Name: must begin with a character") )
    } else if( input$modalAttrName %in% names(getTib()) ){ # check name uniqueness
      showModal(attrValueModal( errMssg="Invalid Column Name: this name is already taken!") )
    } else if(!grepl(pattern = "^[[:graph:]]", input$modalAttrValue) ){  # check value uniqueness
      showModal(attrValueModal( errMssg="Invalid Column Value: must begin with printable character other than space") )
    } else { 
      #add name to tib
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
      
      newPtDefs$tib[[getTibName()]]<-add_column(newPtDefs$tib[[getTibName()]], 
                                                !!(newColName):=input$modalAttrValue   )     
      # updateAce
      
      sender<-'addNewColumn'
      updateAceExtDef(newPtDefs, sender=sender)
      # set selection to this column?
      removeModal() #close dialog
    }
  #}
})

