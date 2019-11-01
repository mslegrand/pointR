rowGroupsDB<-reactiveVal(initialRowGroupDB())

# if the number of rows change or the current row changes
# possible initial triggers causing a change in getTibNRow():
#  1. asset change 
#  2. split button 
#  3. clone button
#  4. delete row button
#  5. user code change
# possible triggers for getTibRow:
#   0. any of the above plus
#   1. rowup button
#   2. rowdown button
#  Generally, 
# the number of rows is determined from ptDefs after ace update 
# so, the control cannot determine the initial trigger 
observeEvent(  getTibRow(), {
  # updateRadioButtons(session, "rowIndex", label = NULL,  choices=1:(getTibNRow()),
  #                    selected = getTibRow())
  rowIndex<-input$myTibRowCntrl$selected
  if(!is.null(getTibRow()) &&
     rowIndex==getTibRow() &&
     !is.null(getTibNRow()) &&
     length(input$myTibRowCntrl$order)== getTibNRow()
  ){
    return(NULL)
  }
  updateRowPicker(session, "myTibRowCntrl",
                  selectRow = getTibRow()
    )
})

observeEvent( getTibNRow(), {
  # updateRadioButtons(session, "rowIndex", label = NULL,  choices=1:(getTibNRow()),
  #                    selected = getTibRow())
  rowIndex<-input$myTibRowCntrl$selected
  if(!is.null(getTibRow()) &&
     rowIndex==getTibRow() &&
     !is.null(getTibNRow()) &&
     length(input$myTibRowCntrl$order)== getTibNRow()
  ){
    return(NULL)
  }
  
  updateRowPicker(session, "myTibRowCntrl",
                  selectRow = getTibRow(),
                  count= getTibNRow()
  )
})



# trigger: this control changes the selected row,
# 
observeEvent( input$myTibRowCntrl$selected, {
  #input$rowIndex,{
  if( getTibEditState()==TRUE ){
    #cat("serverRowDND:: -----Entering-----rowIndex()::----------------\n")
    #rowIndex<-as.integer(input$rowIndex)
    rowIndex<-input$myTibRowCntrl$selected
    if(!is.null(getTibRow()) && rowIndex==getTibRow()){ return(NULL) } #bail
    rowIndex<-min(getTibNRow(),rowIndex)
    # compute matColIndex and update rowIndex, matColIndex
    if(getColumnType()=='point'){
      pts<-getTibPts()
      matColIndex<-length(pts[[rowIndex]])/2
      updateSelected( matCol=matColIndex, rowIndex=rowIndex)
    } else {
      if(!is.null(getPreProcScript()['onChangeRow'])){
        ptDefs<-getPtDefs()
        selection<-getAssetName()
        preprocTrySetAttrValue('onChangeRow', ptDefs, rowIndex, selection, mssg=NULL)
      } else {
        updateSelected( rowIndex=rowIndex)
      }
    }
  }
})

# if this control changes the order
observeEvent( input$myTibRowCntrl$order,{
  if( getTibEditState()==TRUE &  !all(diff(input$myTibRowCntrl$order)==1)){
    ordering<-input$myTibRowCntrl$order
    log.val(ordering)
    name<-getAssetName()
    row<-getTibRow()
    columnName<-getTibColumnName()
    newPtDefs<-getPtDefs()
    tib<-newPtDefs$tib[[name]]
    tib<-tib[ordering,]
    newPtDefs$tib[[name]]<-tib
    row<-which(row==ordering)
    sender="reorderRow"
    updateAceExtDef(
      newPtDefs, sender=sender, 
      selector=list( name=name, rowIndex=row, columnName=columnName   ) 
    )
  }
})

observeEvent( input$myTibRowCntrl$group,{
  if( getTibEditState()==TRUE ){
    group<-input$myTibRowCntrl$group
    aname<-getAssetName()
    pageId<-getTibTabId()
    cname<-getTibColumnName()
    #db<-filter(rowGroupsDB(), tabId!=pageId || name!=aname || colName != cname )
    db<-filter(rowGroupsDB(), tabId!=pageId | name!=aname)
    db<-rbind(db, tibble(tabId=pageId, name=aname,rows=group, colName=cname))
    rowGroupsDB(db)
  }
})


# observeEvent(getAssetName(),{ #reload rowpicker
#   aname<-getAssetName()
#   if(!is.null(aname)){
#     pageId<-input$pages
#     count<-getTibNRow()
#     aname<-getAssetName()
#     cname<-getTibColumnName()
#     row<-getTibRow()
#     group<-filter(rowGroupsDB(), tabId==pageId,  name==aname, colName==cname)$rows
#     updateRowPicker(session, "myTibRowCntrl",
#                     selectRow = row,
#                     count= count,
#                     group=group
#     )    
#   }
# })
# 

# observeEvent(input$page,{
#   rowGroupsDB(initialRowGroupDB())
#   updateRowPicker(session, "myTibRowCntrl",
#                   selectRow=getTibRow(),
#                   count<-getTibNRow()
#                   )
# })
