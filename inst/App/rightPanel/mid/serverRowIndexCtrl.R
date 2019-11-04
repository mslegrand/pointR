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
  rowIndex<-input$myTibRowCntrl$selected
  if(!is.null(getTibRow()) &&
     rowIndex==getTibRow() &&
     !is.null(getTibNRow()) &&
     length(input$myTibRowCntrl$order)== getTibNRow()
  ){
    return(NULL)
  }
  updateRowPicker(session, "myTibRowCntrl",selectRow = getTibRow() )
})

# observeEvent( getTibNRow(), {
#   rowIndex<-input$myTibRowCntrl$selected
#   if(!is.null(getTibRow()) &&
#      rowIndex==getTibRow() &&
#      !is.null(getTibNRow()) &&
#      length(input$myTibRowCntrl$order)== getTibNRow()
#   ){
#     return(NULL)
#   }
# 
#   updateRowPicker(session, "myTibRowCntrl",
#                   selectRow = getTibRow(),
#                   count= getTibNRow()
#   )
# })



# trigger: this control changes the selected row,
# 
observeEvent( input$myTibRowCntrl$selected, {
  #input$rowIndex,{
  if( getTibEditState()==TRUE ){
    log.fin(input$myTibRowCntrl$selected)
    rowIndex<-input$myTibRowCntrl$selected
    if(!is.null(getTibRow()) && rowIndex==getTibRow()){ 
      # group<-input$myTibRowCntrl$group
      # if(length(group)>0)
      #   log.val(format(paste(group,collapse=",")))
      # else
        # cat('group is empty\n')
        # cat('bailing\n')
        #  log.fout(input$myTibRowCntrl$selected)
      return(NULL)  #bail
    }
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
    # log.fout(input$myTibRowCntrl$selected)
  }
})

# if this control changes the order
observeEvent( input$myTibRowCntrl$order,{
  if( getTibEditState()==TRUE &  !all(diff(input$myTibRowCntrl$order)==1)){
    ordering<-input$myTibRowCntrl$order
    # log.val(ordering)
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
    # log.fin(input$myTibRowCntrl$group)
    group<-input$myTibRowCntrl$group
    # if(length(group)>0)
    # log.val(format(paste(group,collapse=",")))
    # else
    #   cat('group is empty\n')
    aname<-getAssetName()
    pageId<-getTibTabId()
    cname<-getTibColumnName()
    # print(rowGroupsDB())
    db<-filter(rowGroupsDB(), tabId!=pageId | name!=aname)
    db<-rbind(db, tibble(tabId=pageId, name=aname,rows=group, colName=cname))
    # cat('setting rowGroupsDB with db=')
    # print(db)
    rowGroupsDB(db)
    # cat('now rowGroupsDB=')
    # print(rowGroupsDB())
    # log.fout(input$myTibRowCntrl$group)
  }
})


observeEvent(getAssetName(),{ #reload rowpicker
  aname<-getAssetName()
  if(!is.null(aname)){
    # log.fin("reload rowpicker")
    # log.val(aname)
    # group<-input$myTibRowCntrl$group
    # if(length(group)>0){
    #   cat('groups=\n')
    #   log.val(format(paste(group,collapse=",")))
    # } else {
    #   cat('group is empty\n')
    # }
    
    # print(rowGroupsDB())
    pageId<-input$pages
    count<-getTibNRow()
    aname<-getAssetName()
    cname<-getTibColumnName()
    
    group<-filter(rowGroupsDB(), tabId==pageId,  name==aname, colName==cname)$rows
    row<-getTibRow()
    if(length(group)>0 && !(row %in% group)){
      row<-tail(group,1)
      updateSelected(rowIndex=row)
    }
      
    #browser()
    # cat('class of group is ',class(group),'\n')
    # updateRowPicker(session, "myTibRowCntrl",
    #                 count= count
    # )
    # cat('************ (getAssetName count=count\n')
    # print(rowGroupsDB())
    # cat('*********before**************\n')
    updateRowPicker(session, "myTibRowCntrl",
                    count= count,
                    selectRow = row,
                    addToGroup=group
    )
    # cat('*********after**************\n')
    # print(rowGroupsDB())
    # if(length(group)>0){
    #   cat('groups=\n')
    #   log.val(format(paste(group,collapse=",")))
    # } else {
    #   cat('group is empty\n')
    # }
    # log.fout("reload picker")
  }
})


observeEvent(input$pages,{
  # log.fin(input$pages)
  # cat('-----initializing rowGroupDB------')
  rowGroupsDB(initialRowGroupDB())
  updateRowPicker(session, "myTibRowCntrl",
                  selectRow=getTibRow(),
                  count<-getTibNRow()
                  )
  # log.fout(input$pages)
})
