

#--------NEW COLUMN------------------------------
addNewColModal <- function(errMssg=NULL, treatAsSelect='string') {
  log.val(treatAsSelect)
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  ppscriptChoices<-unique(preProcScriptDB$attrs$scriptName)
  colChoices<-names(aux$colChoiceSet)
  colChoices<-unique(colChoices)
  choices<-list(  
    ' a single character string'='string','a single number'='number','a vector'='expression',
    'a matrix of points'='points')
  if(length(ppscriptChoices)>0){
    choices<-c(choices,list('using a preprocessing script'='script'))
  }
  if(length(aux$colChoiceSet)>0){
    choices<-c(choices,list('using a choice set'='choiceSet'))
  }
  modalDialog(
    size='l',
    onkeypress=doOk, 
    span('Enter both a name for the new column and a value for its entries'), 
    textInput("modalAttrName", "Enter the name for the new column"),
    div( class='ptR2',id='modalColTreatAsDiv',  
          awesomeRadio('modalColTreatAs', 'Initialize Column Values as ', 
          choices = choices ,
          inline = TRUE,
          selected=treatAsSelect
      )
    ),
    div( 
      textInput("modalAttrValue", "Enter an entry value for the new column" )
    ),
    
    # pick from preproc list
    if(length(ppscriptChoices)>0){
      div( class='ptR2', #awesomeRadio
           pickerInput('modalColPreProcScript', 'Set entry values using the script:', 
                      choices = ppscriptChoices,
                      inline = FALSE
         )
      )
    } else {
      NULL
    },
    if(length(aux$colChoiceSet)>0){
      div( style=paste0("display:inline-block;"),
          div(style="float:left;",pickerInput('modalColChooserSet', 'Choiceset:', 
                       choices = colChoices,
                       inline = FALSE
          )),
          div(style="float:right;",pickerInput('modalColChooserValue', 'Choice value:', 
                      choices = aux$colChoiceSet[[1]],
                      inline = FALSE
          ))
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

is.Non.Blank.Name<-reactiveVal(FALSE)
is.Non.Blank.Value<-reactiveVal(FALSE)

observeEvent(input$modalAttrName,{
  str<-input$modalAttrName
  is.Non.Blank.Name(nchar(str)>0)
})
observeEvent(input$modalAttrValue,{
  str<-input$modalAttrValue
  is.Non.Blank.Value(nchar(str)>0)
})




observeEvent(c(is.Non.Blank.Name(),input$modalColTreatAs, is.Non.Blank.Value() ),{
  if( is.Non.Blank.Name()){
    showElement('modalColTreatAsDiv')
    if(  !(input$modalColTreatAs %in% c('string','number','expression')) || is.Non.Blank.Value() ){
      showElement('commitNewCol')
    } else {
      hideElement('commitNewCol')
    }
    if(input$modalColTreatAs=='points'){
      hideElement('modalAttrValue')
      hideElement('modalColPreProcScript')
      hideElement('modalColChooserSet')
      hideElement('modalColChooserValue')
    } else if (input$modalColTreatAs=='script') {
      hideElement('modalAttrValue')
      showElement('modalColPreProcScript')
      hideElement('modalColChooserSet')
      hideElement('modalColChooserValue')
    } else if (input$modalColTreatAs=='choiceSet') {
      hideElement('modalAttrValue')
      hideElement('modalColPreProcScript')
      showElement('modalColChooserSet')
      showElement('modalColChooserValue')
    } else {
      showElement('modalAttrValue')
      hideElement('modalColPreProcScript')
      hideElement('modalColChooserSet')
      hideElement('modalColChooserValue')
    }
  } else {
    hideElement('modalColTreatAsDiv')
    hideElement('modalAttrValue')
    hideElement('modalColPreProcScript')
    hideElement('modalColChooserSet')
    hideElement('modalColChooserValue')
    hideElement('commitNewCol')
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
    if(!grepl(pattern = "^[[:alpha:]]", input$modalAttrName)){
      # check name syntax
      showModal(addNewColModal( errMssg="Invalid Column Name: must begin with a character", treatAsSelect=treatAs) )
    } else if( input$modalAttrName %in% names(getTib()) ){ 
      # check name uniqueness
      showModal(addNewColModal( errMssg="Invalid Column Name: this name is already taken!", treatAsSelect=treatAs) )
    } else if(
        (!(treatAs %in% c('script', 'points',  'choiceSet' ))) &&
        (!grepl(pattern = "^[[:graph:]]", input$modalAttrValue))
    ){
        # check value is printable
        showModal(addNewColModal( errMssg="Invalid Column Value: must begin with printable character other than space", treatAsSelect=treatAs) )
    } else if( treatAs=='expression' && badExpr( input$modalAttrValue )==TRUE ){
      showModal(addNewColModal( errMssg="Unable to evaluate expression", treatAsSelect=treatAs) )
    } else { # checks passed
      #add name to tib
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
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
              ppenv$context$row<-rowIndex
              ppenv$context$tibs<-tibs
              tibs<-eval(parse(text=txt), ppenv )
              #ppenv$tibs<-tibs

            }
        # 7. check if tibs is valid
          validateTibLists(getPtDefs()$tib, tibs)
          newPtDefs$tib<-tibs
          sender='cmd.add.column'
          #set the column to use specified script
          setPreProcScriptName(tab_Id=getTibTabId(), tib_Name= getAssetName(), column_Name=newColName,  script_Name=script_Name)
          # updateAceExtDef(newPtDefs, sender=sender, selector=list( name=newColName ) ) #NO! THIS UPDATE CAN CAUSE MESSAGING LOOP
        }, error=function(e){
          e<-c('preproErr',e)
          err<-paste(unlist(e), collapse="\n", sep="\n")
          shinyalert("preproc new column Error",err, type="error") # may want to put this in a scrollable modal
        })        
      } else { #not scripting
        if(treatAs=='choiceSet'){
          newVal<-input$modalColChooserValue
          # restrict that value is restiricted to this list
          colSet_Name<-input$modalColChooserSet
          
          #To do: perform additional checks !!!
          #updateWidgetChoicesRow( selectedWidget=returnValue4ModuleEdTib$selectedWidget())
          setColSet4PageName( tab_Id=getTibTabId(), tib_Name= getAssetName(), column_Name=newColName,   colSet_Name=colSet_Name)
        }
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

