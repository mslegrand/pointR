

# --------------input$plotNavBar=="tibEditor"---------------- 

returnValue4ModuleEdTib<-callModule(
  module=moduleEdTib,
  id="tagValBar",
  name=reactive({ 
    if(hasError()){ 
      rtv<-errorPanelTag } else{ 
        rtv<-getAssetName() 
      }
    # cat('Input name to tibEditor is=',format(rtv),'\n')
    rtv
  }),
  nameChoices=reactive({
      rtv<-getRightPanelChoices()
      rtv
  }),
  getRowIndex=reactive({            if( getTibEditState()==TRUE ){ getTibRow() } else { NULL } }),
  getTibNRow=reactive({             if( getTibEditState()==TRUE ){ getTibNRow() } else { NULL } }),
  matColIndex=reactive({            if( getTibEditState()==TRUE ){ getTibMatCol() } else { NULL } }),
  matColIndexChoices=reactive({     if( getTibEditState()==TRUE ){ getTibMatColChoices() } else { NULL } }),
  getMatColIndex=reactive({         if( getTibEditState()==TRUE ){ getTibMatCol() } else { NULL } }),
  getMatColMax=reactive({           if( getTibEditState()==TRUE ){ getTibMatColMax() } else { NULL }}),
  getColumnName= reactive({         if( getTibEditState()==TRUE ){ getTibColumnName() } else { NULL } }),
  getColumnNameChoices=reactive({   if( getTibEditState()==TRUE ){ getTibColumnNameChoices() } else { NULL } }),
  getTibEntry=reactive({            if( getTibEditState()==TRUE ){ getTibEntry() } else { NULL } }),
  getTibEntryChoices=reactive({     if( getTibEditState()==TRUE ){ getTibEntryChoices() } else { NULL } }),
  getTransformType=getTransformType,
  getTibEditState=getTibEditState,
  getWidgetChoices=getWidgetChoices,
  getWidget=getWidget #reactive({  if( getTibEditState()==TRUE ){ getHandlerValue() } else { NULL } })
)


getSafeSelection<-function(selection, choices){ #anybody using this???
  if(is.null(choices)){
    return(NULL)
  }
  if(is.null(selection) || !(selection %in% choices)){
    selection<-unlist(choices)[1]
  }
  selection
}

observeEvent(returnValue4ModuleEdTib$selectedWidget(), {
  if( getTibEditState()==TRUE && !is.null( returnValue4ModuleEdTib$selectedWidget() )){
    log.fin(returnValue4ModuleEdTib$selectedWidget())
    selectedWidget<-returnValue4ModuleEdTib$selectedWidget()
    log.val(selectedWidget)
    updateWidgetChoicesRow( selectedWidget=returnValue4ModuleEdTib$selectedWidget())
    log.fout(returnValue4ModuleEdTib$selectedWidget())
  }
})

observeEvent(returnValue4ModuleEdTib$transformType(),{
  if( getPlotState()==transformTag){
    # cat("\n-----------returnValue4ModuleEdTib$transformType--------------------\n")
      tt<-returnValue4ModuleEdTib$transformType()
      if(!is.null(tt) && tt!=getTransformType() ){
        updateSelected( transformType= tt)
      }
  }
})


# matColIndex --- not integrated back into this version
observeEvent( returnValue4ModuleEdTib$matColIndex() ,{
  if( getTibEditState()==TRUE ){
    matColIndex<-returnValue4ModuleEdTib$matColIndex()
    if( !is.null(matColIndex) ){ #add check for range
      updateSelected( matCol=matColIndex )
    }
  }
}, label='EdTib-rtv-matColIndex')

#  columnName update
observeEvent(returnValue4ModuleEdTib$columnName(),{
  if( getTibEditState()==TRUE ){
    colName<-returnValue4ModuleEdTib$columnName()
    if(!is.null(colName) && nchar(colName)>0 ){
      updateSelected(columnName=colName)
    }
  }
}, label='EdTib-rtv-columnName')



#--------EDIT Entry VALUE------------------------------
observeEvent(returnValue4ModuleEdTib$entryValue(),{
  if( getTibEditState()==TRUE ){
    log.fin(returnValue4ModuleEdTib$entryValue() )
    entry<-returnValue4ModuleEdTib$entryValue()
    # 
    if(length(entry)==0 || is.na(entry) ){
      return(NULL)
    }
    if(identical(getColumnType(),'point')){
      entry<-which(entry==c('point','matrix'))
      if(length(entry)){
        updateSelected(selIndex =entry)
      }
      return(NULL)
    } 
    # format entry according to columnType
    if (identical(getColumnType(),'integer')){
      entry=as.integer(entry)
    } else if (identical(getColumnType(),'numeric')){
      entry=as.numeric(entry)
    } else if (identical(getColumnType(),'logical')){
      entry=as.logical(entry)
    } else if (length(entry)==1 && getColumnType() %in% 
          c("character.list", "character.list.2", "character.list.vec",
          "numeric.list", "numeric.list.2", "integer.list.2", "numeric.list.vec",
          "integer.list.vec")
    ){
        bad<-TRUE
        tryCatch({
         entry<-eval(parse(text=entry)) #TODO!!!!!!!!!!!!! Better Error check???
         bad<-FALSE
        }, error=function(e){})
        if(bad){
          triggerRefresh('cmd.commit') # this works but move to the last row.
          return(NULL) #TODO !!!! force reset dropdown value in modulueEdTib (refresh or commit?)
        }
    }
    name<-getAssetName()
    newPtDefs<-getPtDefs()
    columnName<-getTibColumnName()
    rowIndex<-getTibRow()
    good<-all(!sapply(list(name, newPtDefs, columnName, rowIndex), is.null))
    stopifnot(good)
    tib<-newPtDefs$tib[[name]]
    stopifnot(
        0<rowIndex && 
        !is.null(nrow(tib)) && 
        rowIndex<=nrow(tib)
    )
    sender='applyTibEdit'
   
    if(!identical(newPtDefs$tib[[getAssetName()]][[columnName ]][[rowIndex]],entry)){
      newPtDefs$tib[[getAssetName()]][[columnName]][[rowIndex ]]<-entry
      updateAceExtDef(newPtDefs, sender=sender, selector=list( name=name, rowIndex=rowIndex, columnName=columnName   ) )
    }
    log.fout(returnValue4ModuleEdTib$entryValue())
  }
},label='EdTib-rtv-entryValue', ignoreNULL = TRUE)


observeEvent( returnValue4ModuleEdTib$newColumn(),{
  showModal( addNewColModal() )
}, label='EdTib-rtv-newColumn', ignoreInit = TRUE, ignoreNULL = TRUE)
