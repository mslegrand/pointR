

# --------------input$plotNavBar=="tibEditor"---------------- 

returnValue4ModuleEdTib<-callModule(
  module=moduleEdTib,
  id="tagValBar",
  name=getRightPanelName,
  nameChoices=getRightPanelChoices,
  getRowIndex=reactive({            if( getTibEditState()==TRUE ){ getTibRow() } else { NULL } }),
  getTibNRow=reactive({             if( getTibEditState()==TRUE ){ getTibNRow() } else { NULL } }),
  matColIndex=reactive({            if( getTibEditState()==TRUE ){ getTibMatCol() } else { NULL } }),
  matColIndexChoices=reactive({     if( getTibEditState()==TRUE ){ getTibMatColChoices() } else { NULL } }),
  getColumnName= reactive({         if( getTibEditState()==TRUE ){ getTibColumnName() } else { NULL } }),
  getColumnNameChoices=reactive({   if( getTibEditState()==TRUE ){ getTibColumnNameChoices() } else { NULL } }),
  getTibEntry=reactive({            if( getTibEditState()==TRUE ){ getTibEntry() } else { NULL } }),
  getTibEntryChoices=reactive({     if( getTibEditState()==TRUE ){ getTibEntryChoices() } else { NULL } }),
  getTransformType=getTransformType,
  getTibEditState=getTibEditState,
  getHandler=reactive({  if( getTibEditState()==TRUE ){ getHandler() } else { NULL } }),
  getHandlerValue=getHandlerValue #reactive({  if( getTibEditState()==TRUE ){ getHandlerValue() } else { NULL } })
)


#name
observeEvent(returnValue4ModuleEdTib$name(),{
    name<-returnValue4ModuleEdTib$name()
    if(name==getTibName()){ return(NULL) } #bail if moduleEdTib did not change name
    if(name==transformTag){
      updateSelected(name=transformTag)
    } else if(name==logTag){
        updateSelected(name=logTag)
      } 
    else {
      tibs<-getPtDefs()$tib
      resetSelectedTibbleName(tibs=tibs, name=name)
    }
})

observeEvent(returnValue4ModuleEdTib$transformType(),{
  if( getPlotState()==transformTag){
      tt<-returnValue4ModuleEdTib$transformType()
      if(!is.null(tt) && tt!=getTransformType() ){
        updateSelected( transformType= tt)
      }
  }
})

# rowIndex
# if moduleEdTib changes the rowIndex,  matCol in selectedTibble needs to be updated
observeEvent(returnValue4ModuleEdTib$rowIndex(),{
  if( getTibEditState()==TRUE && !is.null(getTibRow()) ){
    cat("serverEdTib::...Entering----- returnValue4ModuleEdTib$rowIndex()\n")
    rowIndex<-returnValue4ModuleEdTib$rowIndex()
    if(rowIndex==getTibRow()){ return(NULL) } #bail if moduleEdTib did not change rowIndex 
    cat("serverEdTib::...rowIndex=",rowIndex,"\n")
    # moduleEdTib changed rowIndex
    # extract row, column entry from tib
    name<-returnValue4ModuleEdTib$name()
    tib<-name %AND% getPtDefs()$tib[[name]]
    colName<-getTibColumnName()
    indices<-extractSafeRowColIndex(tib, rowIndex, colName)
    if(!is.null(indices)){
      entry<-tib[[indices$rowIndex, indices$colIndex]]
      if( is.matrix(entry) && dim(entry)[1]==2 ){ # need to update the matColIndex
        matColIndex<-ncol(entry)
        updateSelected(name=name, row=rowIndex, matCol=matColIndex)
      } else { #not column of points, so no need to update matColIndex
        updateSelected(name=name, row=rowIndex)
      }
    }
    cat("serverEdTib::...Exiting----- returnValue4ModuleEdTib$rowIndex()\n")
  }
})



# matColIndex
observeEvent( returnValue4ModuleEdTib$matColIndex() ,{
  if( getTibEditState()==TRUE ){
    matColIndex<-returnValue4ModuleEdTib$matColIndex()
    if( !is.null(matColIndex) ){ #add check for range
      updateSelected( matCol=matColIndex )
    }
  }
}, label='EdTib-rtv-matColIndex')

#  columnName
observeEvent(returnValue4ModuleEdTib$columnName(),{
  if( getTibEditState()==TRUE ){
    #cat('serverEdTib::...Entering-----returnValue4ModuleEdTib$columnName()\n')
    colName<-returnValue4ModuleEdTib$columnName()
    #cat('serverEdTib::...colName=',colName,".\n")
    if(!is.null(colName) && nchar(colName)>0 ){
      updateSelected(columnName=colName)
    }
    #cat('serverEdTib::...Exiting-----returnValue4ModuleEdTib$columnName()\n')
  }
}, label='EdTib-rtv-columnName')



#--------EDIT VALUE------------------------------



observeEvent(returnValue4ModuleEdTib$entryValue(),{
  if( getTibEditState()==TRUE ){
    #cat("serverEdTib::...Entering----- returnValue4ModuleEdTib$entryValue()\n")
    # assuming tib is uptodate, simply work on the existing tib
    name<- returnValue4ModuleEdTib$name()
    #cat('serverEdTib::...name=', name,"\n")
    entry<-name %AND% returnValue4ModuleEdTib$entryValue()
    # cat('serverEdTib::...entry=', entry,"\n")
    # print(returnValue4ModuleEdTib$entryValue())
    row= entry %AND% returnValue4ModuleEdTib$rowIndex()
    # cat('serverEdTib::row=', row,"\n")
    columnName<-row %AND% returnValue4ModuleEdTib$columnName()
    #cat('serverEdTib::...columnName=', columnName,"\n")
    if(!is.null(columnName) && nchar(entry) &&
       !is.null(getTibColumnName()) && columnName==getTibColumnName()){
      
      # this is where we handle points/matrix
      if(!(entry %in% c('matrix','point'))){
        newPtDefs<-getPtDefs()
        column<-columnName
        # !!! todo: refactor
        #should be exactly the same as returnValue4ModuleEdTib$columnName()
        # so column and columnName are redundent
        #row<-newPtDefs$tib[[name]] %AND% getTibRow()
        row<-newPtDefs$tib[[name]] %AND% row
        if(isNumericString(entry)){
          entry<-as.numeric(entry)
        }
        if(!is.null(row) && row>=1 && row<=nrow(newPtDefs$tib[[name]])){
          sender='applyTibEdit'
          newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, columnName=columnName   ) )
        }
      } else {
        if(length(which(entry==c('point','matrix')) )==0){
          stop('entry null')
        }
        cat("serverEdTib::...entry:    ", entry ,"\n")
        updateSelected( selIndex = which(entry==c('point','matrix')) )
      }
    }
    cat("serverEdTib::...Exiting:    returnValue4ModuleEdTib$entryValue()\n")
  } 
},label='EdTib-rtv-entryValue')


#-------points----------------------------------------------

#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----

#----begin for Tagging-------------------------------------



#---TAG THIS POINT button-----


# observeEvent( returnValue4ModuleEdTib$input$resetColInput(), {
#   # here we get the colName and the column type
#   colName<-getTibColumnName()
#   tib<-getTib()
#   colValues<-tib[[colName]]
#   if(isColorString(colValues)){ # if column is colors , colorOption dialog
#     # bring up color modal    
#     # user selects where or not to use color palette
#     # set ptR$widget[[name]][colName]='colorpalette' 
#   }
#   
#   # if col is ints, int dialog
#   # if col is ...
# })

