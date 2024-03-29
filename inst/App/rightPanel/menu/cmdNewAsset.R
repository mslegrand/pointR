#--------NEW COLUMN------------------------------
addNewAssetModal <- function(errMssg=NULL ) {
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  choices<-c('matrix','tibble')
  if(identical(getParMode(),"dnippets")){
    choices<-'matrix'
  }
  modalDialog(
    onkeypress=doOk, 
    span('Enter both the type and the name for the new asset'), 
    textInput("modalAssetName", "Enter the name for the new asset"),
    div( class='ptR2',
    awesomeRadio("modalAssetType", "Asset Type", 
                 choices=choices
    )), 
    if(!is.null(errMssg)){
      div(tags$b(errMssg, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("commitNewAsset", "Commit")
    )
  ) 
}

observeEvent(input$commitNewAsset, {
  #checks 
  #if(!grepl(pattern = "^[[:alpha:]]", input$modalAssetName)){ # check name syntax
  newAssetName<-input$modalAssetName
  
  forbidden<-c(names(getPtDefs()$tib),  'svgPanel', 'RPanel')
  if(!goodRName(newAssetName ) ){ # check name syntax
    showModal(addNewAssetModal( errMssg="Invalid Asset Name: must begin with a character") )
  } else if( newAssetName %in% forbidden ){ # check name uniqueness
    showModal(addNewAssetModal( errMssg="Invalid Asset Name: this name is already taken!") )
  } else { 
    #add name to tib
    newPtDefs<-getPtDefs()
    newPtDefs$tib[[newAssetName]]<-tibble(points=list(matrix(0,2,0)))
    newPtDefs$mats[newAssetName]<-input$modalAssetType=='matrix' 

    # updateAce and set selection to this column
    #sender<-'cmd.add.column'
    sender<-'cmd.add.asset'
    updateAceExtDef(newPtDefs, sender=sender, selector=list( name = newAssetName   ) )
    
    removeModal() #close dialog
  }
})

