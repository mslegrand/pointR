

#--------NEW COLUMN------------------------------
addNewColModal <- function(errMssg=NULL) {
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  ppscriptChoices<-unique(preProcScriptDB$attrs$scriptName)
  modalDialog(
    onkeypress=doOk, 
    span('Enter both a name for the new column and a value for its entries'), 
    textInput("modalAttrName", "Enter the name for the new column"),
     div( class='ptR2',
       awesomeRadio('modalColTreatAs', 'Initialize Column Values as ', 
          choices = list(  
            ' a character string'='string','a number'='number','an expression'='expression',
            'a matrix of points'='points' #, 'the preprocessing script result'='script'
            ) ,
          inline = TRUE
      )
    ),
    textInput("modalAttrValue", "Enter an entry value for the new column"),
    # pick from preproc list
    if(length(ppscriptChoices)>0){
      div( class='ptR2',
         awesomeRadio('modalColPreProcScript', 'Set entry values using the script:', 
                      choices = ppscriptChoices,
                      inline = TRUE
         )
      )
    } else {
      NULL
    },
   
    if(!is.null(errMssg)){
      div(tags$b(errMssg, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("commitNewCol", "Commit")
    )
  ) 
}


observeEvent(input$modalColTreatAs,{
  if(input$modalColTreatAs=='points'){
    hideElement('modalAttrValue')
    hideElement('modalColPreProcScript')
  } else if (input$modalColTreatAs=='script') {
    hideElement('modalAttrValue')
    showElement('modalColPreProcScript')
  } else {
    showElement('modalAttrValue')
    hideElement('modalColPreProcScript')
  }
})

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
    } else if( treatAs=='expression' && badExpr( input$modalAttrValue )==TRUE ){
      showModal(addNewColModal( errMssg="Unable to evaluate expression") )
    } else { 
      #add name to tib
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
      
      #treatAs<-input$modalColTreatAs
      #newVal<-input$modalAttrValue
      if(treatAs=='script'){ # apply script sequentially to newPtDefs
        # extract onNewRowScript
        script_Name<-input$modalColPreProcScript
        tb<-filter(preProcScriptDB$attrs, scriptName==script_Name)
        scripts<-unlist(tb$script)
        names(scripts)<-tb$cmd
        onNewRowScript<-scripts['onNewRow'] 
        # signature of onNewRowScript is pt, context, WH, keys
        # But keys, pt, WH, are likey IRREVELANT
        # A executaion environment ppenv is provided which includes
            # setAttrValue=setAttrValue,
            # getAttrValue=getAttrValue,
            # getLastRow=getLastRow,
            # replaceLastRow=replaceLastRow,
            # appendLastRow=appendLastRow,
            # appendAttrValues=appendAttrValues,
            # context=context,
            # keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey, mssg$keycode)
        
        
        # make a copy of ptDefs
        newTibs<-newPtDefs$tib
        tib<-newPtDefs$tib[[getAssetName()]]
        newTib<-tib[0,]
        newTibs[[newColName ]]<-newTib
        nr=nrows(tib ) 
        # we build newTibs row at a time.
        for(i in 1:nr){
         # tibsN
          # script can add
          # newColVal<- eval(script, newTibs)
          # newTibCol<-c(newTibCol, newColVal )
          # newTib<-cbind(tib[1:i,], newColName = newColVal ) 
          # newTibs[[newColName]]<-newTib
 
        }

      } else {
        if(treatAs=='number'){
          newVal<-as.numeric(newVal)
        } else if ( treatAs=='points'){
          newVal<-list(matrix(0,2,0)) 
        } else if ( treatAs=='expression'){
          newVal<-list(eval(parse(text=newVal))) # to do: validate!!!
        }
  
        newPtDefs$tib[[getAssetName()]]<-add_column(newPtDefs$tib[[getAssetName()]], 
                                                  !!(newColName):=newVal   )   
      }
        
      # updateAce and set selection to this column
      sender<-'cmd.add.column'
      updateAceExtDef(newPtDefs, sender=sender, selector=list( columnName = newColName   ) )
      
      removeModal() #close dialog
    }
})

