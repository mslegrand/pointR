

#--------NEW COLUMN------------------------------
addNewColModal <- function(errMssg=NULL) {
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  modalDialog(
    onkeypress=doOk, 
    span('Enter both a name for the new column and a value for its entries'), 
    textInput("modalAttrName", "Enter the name for the new column"),
    textInput("modalAttrValue", "Enter an entry value for the new column"),
    div( class='ptR2',
       awesomeRadio('modalColTreatAs', 'Treat as ', choices = c('a character string','a number','an expression') )
    ),
   
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
     
  badExpr<-function(txt){
    rtv<-TRUE
    tryCatch({
      eval(parse(text=txt))
      rtv<-FALSE
    }, 
    error = function(e) {})
    rtv
  }
  
  treatAs<-input$modalColTreatAs
  newVal<-input$modalAttrValue
  #checks
    if(!grepl(pattern = "^[[:alpha:]]", input$modalAttrName)){ # check name syntax
      showModal(addNewColModal( errMssg="Invalid Column Name: must begin with a character") )
    } else if( input$modalAttrName %in% names(getTib()) ){ # check name uniqueness
      showModal(addNewColModal( errMssg="Invalid Column Name: this name is already taken!") )
    } else if(!grepl(pattern = "^[[:graph:]]", input$modalAttrValue) ){  # check value uniqueness
      showModal(addNewColModal( errMssg="Invalid Column Value: must begin with printable character other than space") )
    } else if( treatAs=='an expression' && badExpr( input$modalAttrValue )==TRUE ){
      showModal(addNewColModal( errMssg="Unable to evaluate expression") )
    } else { 
      #add name to tib
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
      
      #treatAs<-input$modalColTreatAs
      #newVal<-input$modalAttrValue
      if(treatAs=='a number'){
        newVal<-as.numeric(newVal)
      } else if ( treatAs=='an expression'){
        newVal<-list(eval(parse(text=newVal))) # to do: validate!!!
      }

      newPtDefs$tib[[getTibName()]]<-add_column(newPtDefs$tib[[getTibName()]], 
                                                !!(newColName):=newVal   )     
      # updateAce and set selection to this column
      sender<-'cmd.add.column'
      updateAceExtDef(newPtDefs, sender=sender, selector=list( columnName = newColName   ) )
      
      removeModal() #close dialog
    }
})

