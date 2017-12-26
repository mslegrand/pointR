

# --------------input$plotNavBar=="tibEditor"---------------- 

returnValue4ModuleEdTib<-callModule(
  module=moduleEdTib,
  id="tagValBar",
  id2="tagValBar", # !!! DO  WE STILL NEED THIS???? 
  barName=rightPanel ,
  name=getTibName,
  nameChoices=getTibNameChoices,
  rowIndex=getTibRow,
  rowIndexChoices=getTibRowChoices,
  matColIndex=getTibMatCol,
  matColIndexChoices=getTibMatColChoices,
  columnName= getTibColumnName,
  columnNameChoices=getTibColumnNameChoices,
  getTibEntry=getTibEntry,
  getTibEntryChoices=getTibEntryChoices,
  headerId=NS("tagValBar", 'header')
)


#name
observeEvent(returnValue4ModuleEdTib$name(),{
  if(rightPanel()=='tibEditor'){
    # cat('\n----Entering-----------oE 2-123\n')
    name<-returnValue4ModuleEdTib$name()
    if(name==getTibName()){ return(NULL) } #bail if moduleEdTib did not change name
    
    # name was changed by moduleEdTib
    # this invalidates all entries in selectedTibble
    tibs<-getPtDefs()$tib
    resetSelectedTibbleName(name, tibs)
  }
})


# rowIndex
# if moduleEdTib changes the rowIndex,  matCol in selectedTibble needs to be updated
observeEvent(returnValue4ModuleEdTib$rowIndex(),{
  if(rightPanel()=='tibEditor'){
    rowIndex<-returnValue4ModuleEdTib$rowIndex()
    if(rowIndex==getTibRow()){ return(NULL) } #bail if moduleEdTib did not change rowIndex 
    
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
  }
})



# matColIndex
observeEvent( returnValue4ModuleEdTib$matColIndex() ,{
  if(rightPanel()=='tibEditor'){
    matColIndex<-returnValue4ModuleEdTib$matColIndex()
    if( !is.null(matColIndex) ){ #add check for range
      updateSelected( matCol=matColIndex )
    }
  }
})

#  columnName
observeEvent(returnValue4ModuleEdTib$columnName(),{
  if(rightPanel()=='tibEditor'){
    colName<-returnValue4ModuleEdTib$columnName()
    if(!is.null(colName) && nchar(colName)>0 ){
      updateSelected(columnName=colName)
    }
  }
})



#--------EDIT VALUE------------------------------
observeEvent(returnValue4ModuleEdTib$entryValue(),{
  if(rightPanel()=='tibEditor'){
    cat('\n--------Entering---------entryVAlue\n')
    cat('oE 2-125\n')
    
    # assuming tib is uptodate, simply work on the existing tib
    name<- returnValue4ModuleEdTib$name() 
    entry<-name %AND% returnValue4ModuleEdTib$entryValue()
    
    cat('class(name)=',class(name),'\n')
    
    #rowIndex<-returnValue4ModuleEdTib$rowIndex()
    row= entry %AND% returnValue4ModuleEdTib$rowIndex()
    columnName<-row %AND% returnValue4ModuleEdTib$columnName()
    cat('row=',row,"\n")
    cat("columnName=" ,columnName,"\n")
    
    cat('entry=',entry,'\n')
    
    if(!is.null(columnName) && 
       #length(entry)>0 &&
       nchar(entry)>0   ){
      #entry<-returnValue4ModuleEdTib$entryValue()
      # !!! TODO: type check if numeric
      rP<-rightPanel()
      if(is.null(rP))rP<-'NULL'
      cat('rightPanel=',rP,"\n")
      cat('class(entry)=',class(entry),"\n")
      cat('entry=',entry,"\n")
      cat('setting entry state\n')
      setPlotState(entry)
      if(!(entry %in% c('matrix','point'))){
        cat('inside entry state\n')
        name<-getTibName()
        newPtDefs<-getPtDefs()
        column<-getTibColumnName()
        row<-newPtDefs$tib[[name]] %AND% getTibRow()
        if(!is.null(row) && row>=1 && row<=nrow(newPtDefs$tib[[name]]) ){
          sender='applyTibEdit'
          newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
          updateAceExtDef(newPtDefs, sender=sender)
          updateSelected( name=name, columnName=columnName, row=row)
        }
      } 
    }
    isolate({
      cat('--------Exiting---------entryVAlue\n')
    })
  }
})

observeEvent(
  returnValue4ModuleEdTib$tagClone(),
  {
    cat('oE 2-123\n')
    #if(rightPanel()=="tagDrag"){
    sender='cloneRow'
    ptDefs<-getPtDefs()
    name<-getTibName()
    tib<-ptDefs$tib[[name]]
    rowIndex<-getTibRow()
    newTib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
    rowIndx=rowIndex+1
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    
    pts<-newTib[[getTibPtColPos()]]
    
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs
    updateAceExtDef(newPtDefs, sender=sender)
    updateSelected(row=rowIndex, matCol=matCol)
  }
  #}
)

observeEvent(
  returnValue4ModuleEdTib$tagDelete(),
  {
    #if(rightPanel()=="tagDrag"){
    sender='deleteRow'
    ptDefs<-getPtDefs()
    name<-getTibName()
    newTib<-ptDefs$tib[[name]]
    rowIndex<-getTibRow()
    
    # !!!TODO handle case where this would be last row.
    newTib<-newTib[-rowIndex,]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs
    
    #adjust position
    rowIndex<-min(rowIndex, nrow(newTib))
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender)
    updateSelected(row=rowIndex, matCol=matCol)
  }
  #}
)

observeEvent( returnValue4ModuleEdTib$tagMoveUp(),{ 
  #if(rightPanel()=="tagDrag"){
  rowIndex<-getTibRow()
  if(rowIndex>1){
    sender='tagMoveUp'
    ptDefs<-getPtDefs()
    name<-    getTibName()
    newTib<-ptDefs$tib[[name]]
    
    newTib[c(rowIndex,rowIndex-1),]<-newTib[c(rowIndex-1,rowIndex),]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs
    
    #adjust position
    rowIndex<-rowIndex-1
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender)
    updateSelected(row=rowIndex, matCol=matCol)   
  }
  #}
})

observeEvent( returnValue4ModuleEdTib$tagMoveDown(),{ 
  #if(rightPanel()=="tagDrag"){
  rowIndex<-getTibRow()
  ptDefs<-getPtDefs()
  name<-    getTibName()
  newTib<-ptDefs$tib[[name]]
  if(rowIndex<nrow(newTib)){
    sender='tagMoveDown'
    
    newTib[c(rowIndex,rowIndex+1),]<-newTib[c(rowIndex+1,rowIndex),]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs
    
    #adjust position
    rowIndex<-rowIndex+1
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender)
    updateSelected(row=rowIndex, matCol=matCol)   
  }
  #}
})


#-------points----------------------------------------------

#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( returnValue4ModuleEdTib$removePt(), {
  selection<-getTibName() 
  cat('Enter removePt\n')  
  if(selection!=""){
    ptDefs<-getPtDefs()
    if(length(ptDefs$tib)==0){return(NULL)}
    matCol<-getTibMatCol()
    #src<-getCode() 
    
    #get row, col
    if(matCol>=1){ 
      row<-getTibRow()
      m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]][,-matCol] 
      #!!! probably need some checking here
      ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m
      matCol<-min(matCol, length(m)/2)
      newPtDefs<-ptDefs
      sender='points.deletePoint'
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(matCol=matCol)
    }
  }
}) #end remove point observer

#----begin for Tagging-------------------------------------



#---TAG THIS POINT button-----
observeEvent( returnValue4ModuleEdTib$tagPt(), {
  cat("Enter tagPt\n")
  src<-getCode() 
  selection<-getTibName()
  ptDefs<-getPtDefs()
  
  row=getTibRow() 
  matCol=getTibMatCol() 
  
  m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
  if(ncol(m)<1){ 
    return(NULL) # bail if matrix of points is empty
  }
  tib<-ptDefs$tib[[selection]] #get the tib 
  tib<-tagTib(tib, getTibPtColPos(), row, matCol)
  row<-row+1
  matCol<-length(tib[[row, getTibPtColPos()]])/2
  ptDefs$tib[[selection]]<-tib 
  sender='tagPt'
  updateAceExtDef(ptDefs, sender=sender)
  updateSelected(row=row, matCol=matCol)
}) #end of point InfoList Tag Point, 


# forward point
observeEvent( returnValue4ModuleEdTib$forwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length( matColIndex)>0 && length(matColChoices)>0){
    matColIndex=min(matColIndex+1, max(matColChoices) )
    updateSelected(  matCol=matColIndex )
  }
})
# backward point
observeEvent( returnValue4ModuleEdTib$backwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length(matColIndex)>0 && length(matColChoices)>0){
    cat("observeEvent:: serverPlotBar 98\n")
    matColIndex=max(matColIndex-1, min(matColChoices) )
    updateSelected(  matCol=matColIndex  )
  }
})

