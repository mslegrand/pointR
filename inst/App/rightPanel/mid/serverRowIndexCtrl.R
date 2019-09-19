

observeEvent( c(getTibNRow(), getTibRow()), {
  updateRadioButtons(session, "rowIndex", label = NULL,  choices=1:(getTibNRow()),
                     selected = getTibRow())
})

observeEvent( input$rowIndex,{
  if( getTibEditState()==TRUE ){
    #cat("serverRowDND:: -----Entering-----rowIndex()::----------------\n")
    rowIndex<-as.integer(input$rowIndex)
    if(!is.null(getTibRow()) && rowIndex==getTibRow()){ return(NULL) } #bail
    # compute matColIndex and update rowIndex, matColIndex
    if(getColumnType()=='point'){
      pts<-getTibPts()
      matColIndex<-length(pts[[rowIndex]])/2
      updateSelected( matCol=matColIndex, rowIndex=rowIndex)
    } else {
      if(hasPtScript() && !is.null(getPreProcPtScript()['onChangeRow'])){
        ptDefs<-getPtDefs()
        selection<-getAssetName()
        preprocTrySetAttrValue('onChangeRow', ptDefs, rowIndex, selection)
      } else {
        updateSelected( rowIndex=rowIndex)
      }
    }
  }
})

observeEvent( input$rowIndex_order,{
  if( getTibEditState()==TRUE ){
    ordering<-as.integer(input$rowIndex_order)
    name<-getAssetName()
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
