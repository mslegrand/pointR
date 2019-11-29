

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
            'a matrix of points'='points' , 'the preprocessing script result'='script'
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
  # browser()
  treatAs<-input$modalColTreatAs
  newVal<-input$modalAttrValue
  #checks
    if(!grepl(pattern = "^[[:alpha:]]", input$modalAttrName)){
      # check name syntax
      showModal(addNewColModal( errMssg="Invalid Column Name: must begin with a character") )
    } else if( input$modalAttrName %in% names(getTib()) ){ 
      # check name uniqueness
      showModal(addNewColModal( errMssg="Invalid Column Name: this name is already taken!") )
    } else if(
        (!(treatAs %in% c('script', 'points'  ))) &&
        (!grepl(pattern = "^[[:graph:]]", input$modalAttrValue))
    ){
        # check value is printable
        showModal(addNewColModal( errMssg="Invalid Column Value: must begin with printable character other than space") )
    } else if( treatAs=='expression' && badExpr( input$modalAttrValue )==TRUE ){
      showModal(addNewColModal( errMssg="Unable to evaluate expression") )
    } else { # checks passed
      #add name to tib
      cat('treatAs=', format(treatAs),'\n')
      cat('newVal=', format(newVal),'\n')
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
      # browser()
      if(treatAs=='script'){ # apply script sequentially to newPtDefs
        # extract onNewRowScript
        script_Name<-input$modalColPreProcScript
        tb<-filter(preProcScriptDB$attrs, scriptName==script_Name)
        scripts<-unlist(tb$script)
        names(scripts)<-tb$cmd
        txt<-scripts['onNewRow'] 
        # signature of onNewRowScript is pt, context, WH, keys
        # But keys, pt, WH, are likey IRRELEVANT
        #One strategy
        tryCatch({
        # 1. newPtDefs<-getPtDefs()
            #newPtDefs<-getPtDefs()
            #browser()
            
        # 2. 
            tibs<-newPtDefs$tib
        # 3. 
            tib<-tibs[[getAssetName()]]
        # 4. 
            tib[[newColName]]<-NA
            tibs[[getAssetName()]]<-tib
        # 5. 
            context<-list(
              name=getAssetName(),
              column=ncol(tib),
              row=1,
              tibs=tibs
            )
        #6. 
            ppenv<-list(
              setAttrValue=setAttrValue,
              getAttrValue=function(){NA},
              getLastRow=getLastRow,
              replaceLastRow=replaceLastRow,
              appendLastRow=appendLastRow,
              appendAttrValues=appendAttrValues,
              context=context,
              keys=list(altKey=FALSE, shiftKey=FALSE, ctrlKey=FALSE, metaKey=FALSE, keycode=NULL)
            )
            for(rowIndex in 1:nrow(tib)){
              #browser()
              ppenv$context$row<-rowIndex
              ppenv$context$tibs<-tibs
              tibs<-eval(parse(text=txt), ppenv )
              #ppenv$tibs<-tibs
            }
        # 7. check if tibs is valid
          validateTibLists(getPtDefs()$tib, tibs)
          newPtDefs$tib<-tibs
          sender='cmd.add.column'
          #browser()
          #updateAceExtDef(newPtDefs, sender=sender, selector=list( name=newColName ) )
        }, error=function(e){
          e<-c('preproErr',e)
          err<-paste(unlist(e), collapse="\n", sep="\n")
          alert(err)
        })        
      } else { #not scripting
        # browser()
        if(treatAs=='number'){
          newVal<-as.numeric(newVal)
        } else if ( treatAs=='points'){
          newVal<-list(matrix(0,2,0)) 
        } else if ( treatAs=='expression'){
          newVal<-list(eval(parse(text=newVal))) # to do: validate!!!
        }
        # newVal is ready to insert
        newPtDefs$tib[[getAssetName()]]<-add_column(newPtDefs$tib[[getAssetName()]], 
                                                  !!(newColName):=newVal   )   
      }
        
      # updateAce and set selection to this column
      sender<-'cmd.add.column'
      updateAceExtDef(newPtDefs, sender=sender, selector=list( columnName = newColName   ) )
      
      removeModal() #close dialog
    }
})

