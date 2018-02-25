
returnValue4ModuleRowDND<-callModule(
  module=moduleRowDND,
  id="rowDND",
  getTibNRow= getTibNRow,  
  getRowIndex=getTibRow
)

# rowIndex
# if moduleEdTib changes the rowIndex,  matCol in selectedTibble needs to be updated
observeEvent(returnValue4ModuleRowDND$rowIndex(),{
  if( getTibEditState()==TRUE ){
    cat("serverRowDND:: -----Entering-----rowIndex()::----------------\n")
    rowIndex<-as.integer(returnValue4ModuleRowDND$rowIndex())
    if(!is.null(getTibRow()) && rowIndex==getTibRow()){ return(NULL) } #bail
    # compute matColIndex and update rowIndex, matColIndex
    if(getColumnType()=='point'){
      pts<-getTibPts()
      matColIndex<-length(pts[[rowIndex]])/2
      # cat(
      #   "updateSelected( matCol=",
      #   format(matColIndex) ,
      #   ", rowIndex=",
      #   format(rowIndex) ,
      #   ")\n"
      # )
      updateSelected( matCol=matColIndex, rowIndex=rowIndex)
    } else {
      # cat(
      #   "updateSelected( ",
      #   "rowIndex=",
      #   format(rowIndex) ,
      #   ")\n"
      # )
      updateSelected( rowIndex=rowIndex)
    }
    #cat("serverEdTib:: -----Leaving-----rowIndex()::----------------\n")
  }
}, ignoreNULL = TRUE)

#row reordering
observeEvent( returnValue4ModuleRowDND$rowReorder() ,{
  if( getTibEditState()==TRUE ){
    ordering<-as.numeric(returnValue4ModuleRowDND$rowReorder())
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
