

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
  getMatColIndex=reactive({            if( getTibEditState()==TRUE ){ getTibMatCol() } else { NULL } }),
  getMatColMax=reactive({ if( getTibEditState()==TRUE ){ getTibMatColMax() } else { NULL }}),
  getColumnName= reactive({         if( getTibEditState()==TRUE ){ getTibColumnName() } else { NULL } }),
  getColumnNameChoices=reactive({   if( getTibEditState()==TRUE ){ getTibColumnNameChoices() } else { NULL } }),
  getTibEntry=reactive({            if( getTibEditState()==TRUE ){ getTibEntry() } else { NULL } }),
  getTibEntryChoices=reactive({     if( getTibEditState()==TRUE ){ getTibEntryChoices() } else { NULL } }),
  getTransformType=getTransformType,
  getTibEditState=getTibEditState,
  getWidgetChoices=getWidgetChoices,
  getWidget=getWidget #reactive({  if( getTibEditState()==TRUE ){ getHandlerValue() } else { NULL } })
)


getSafeSelection<-function(selection, choices){
  if(is.null(choices)){
    return(NULL)
  }
  if(is.null(selection) || !(selection %in% choices)){
    selection<-unlist(choices)[1]
  }
  selection
}

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

observeEvent(returnValue4ModuleEdTib$selectedWidget(), {
  cat("-----------returnValue4ModuleEdTib$selectedWidget\n")
  if( getTibEditState()==TRUE && !is.null(returnValue4ModuleEdTib$selectedWidget)){
    cat("returnValue4ModuleEdTib$selectedWidget= ",format(returnValue4ModuleEdTib$selectedWidget() ),"\n")
    updateWidgetChoicesRow( selectedWidget=returnValue4ModuleEdTib$selectedWidget())
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
  if( getTibEditState()==TRUE ){
    cat("serverEdTib:: -----Entering-----rowIndex()::----------------\n")
    rowIndex<-as.integer(returnValue4ModuleEdTib$rowIndex())
    if(!is.null(getTibRow()) && rowIndex==getTibRow()){ return(NULL) } #bail
    # compute matColIndex and update rowIndex, matColIndex
    if(getColumnType()=='point'){
        pts<-getTibPts()
        matColIndex<-length(pts[[rowIndex]])/2
        cat(
          "updateSelected( matCol=",
          format(matColIndex) ,
          ", rowIndex=",
          format(rowIndex) ,
          ")\n"
        )
        updateSelected( matCol=matColIndex, rowIndex=rowIndex)
    } else {
      cat(
        "updateSelected( ",
        "rowIndex=",
        format(rowIndex) ,
        ")\n"
      )
      updateSelected( rowIndex=rowIndex)
    }
    cat("serverEdTib:: -----Leaving-----rowIndex()::----------------\n")
  }
}, ignoreNULL = TRUE)

#row reordering
observeEvent( returnValue4ModuleEdTib$rowReorder() ,{
  if( getTibEditState()==TRUE ){
    ordering<-as.numeric(returnValue4ModuleEdTib$rowReorder())
    cat(paste(ordering,collapse=", "))
    cat("\n")
    name<-getTibName()
    row<-getTibRow()
    columnName<-getTibColumnName()
    newPtDefs<-getPtDefs()
    tib<-newPtDefs$tib[[name]]
    tib<-tib[ordering,]
    newPtDefs$tib[[name]]<-tib
    row<-which(row==ordering)
    sender="reorderRow"
    updateAceExtDef(newPtDefs, sender=sender, selector=list( name=name, rowIndex=row, columnName=columnName   ) )
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
    cat("serverEdTib::...Entering----- returnValue4ModuleEdTib$entryValue()\n")
    entry<-returnValue4ModuleEdTib$entryValue()
    if(length(entry)==0 || is.na(entry) ){
      return(NULL)
    }
    if(getColumnType()=='point'){
      entry<-which(entry==c('point','matrix'))
      if(length(entry)){
        updateSelected(selIndex =entry)
      }
    } else {
      if(isNumericString(entry)){
        entry<-as.numeric(entry)
      }
      name<-getTibName()
      newPtDefs<-getPtDefs()
      columnName<-getTibColumnName()
      rowIndex<-getTibRow()
      sapply(c("name", "columnName", "rowIndex", "entry"), function(x){
        cat(x,"=", format(get(x)),"\n")
      })
      good<-all(!sapply(list(name, newPtDefs, columnName, rowIndex), is.null))
      stopifnot(good)
      tib<-newPtDefs$tib[[name]]
      stopifnot(
          0<rowIndex && 
          !is.null(nrow(tib)) && 
          rowIndex<=nrow(tib)
      )
      sender='applyTibEdit'
      
      #newPtDefs$tib[[name]][[columnName ]][[row]]<-entry
      newPtDefs$tib[[getTibName()]][[rowIndex,columnName ]]<-entry
      updateAceExtDef(newPtDefs, sender=sender, selector=list( name=name, rowIndex=rowIndex, columnName=columnName   ) )
    }
    
    # # assuming tib is uptodate, simply work on the existing tib
    # name<- returnValue4ModuleEdTib$name()
    # cat('serverEdTib::...name=', name,"\n")
    # entry<-name %AND% returnValue4ModuleEdTib$entryValue()
    # cat('serverEdTib::...entry=', entry,"\n")
    # # print(returnValue4ModuleEdTib$entryValue())
    # row= getTibRow() # entry %AND% returnValue4ModuleEdTib$rowIndex()
    # cat('serverEdTib::row=', row,"\n")
    # columnName<-getTibColumnName() #row %AND% returnValue4ModuleEdTib$columnName()
    # cat('serverEdTib::...columnName=', columnName,"\n")
    # if(!is.null(columnName) && nchar(entry) &&
    #    !is.null(getTibColumnName()) && columnName==getTibColumnName()){
    #   
    #   # this is where we handle points/matrix
    #   if(!(entry %in% c('matrix','point'))){
    #     newPtDefs<-getPtDefs()
    #     column<-columnName
    #     # !!! todo: refactor
    #     #should be exactly the same as returnValue4ModuleEdTib$columnName()
    #     # so column and columnName are redundent
    #     #row<-newPtDefs$tib[[name]] %AND% getTibRow()
    #     row<-newPtDefs$tib[[name]] %AND% row
    #     if(isNumericString(entry)){
    #       entry<-as.numeric(entry)
    #     }
    #     if(!is.null(row) && row>=1 && row<=nrow(newPtDefs$tib[[name]])){
    #       sender='applyTibEdit'
    #       newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
    #       updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, columnName=columnName   ) )
    #     }
    #   } else {
    #     if(length(which(entry==c('point','matrix')) )==0){
    #       stop('entry null')
    #     }
    #     cat("serverEdTib::...entry:    ", entry ,"\n")
    #     updateSelected( selIndex = which(entry==c('point','matrix')) )
    #   }
    # }
    # cat("serverEdTib::...Exiting:    returnValue4ModuleEdTib$entryValue()\n")
  } 
},label='EdTib-rtv-entryValue', ignoreNULL = TRUE)


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

