

# --------------input$plotNavBar=="tibEditor"---------------- 

returnValue4ModuleEdTib<-callModule(
  module=moduleEdTib,
  id="tagValBar",
  id2="tagValBar", # !!! DO  WE STILL NEED THIS???? 
  
  name=getRightPanelName,
  nameChoices=getRightPanelChoices,
  rowIndex=reactive({            if( tibEditState()==TRUE ){ getTibRow() } else { NULL } }),
  rowIndexChoices=reactive({     if( tibEditState()==TRUE ){ getTibRowChoices() } else { NULL } }),
  matColIndex=reactive({         if( tibEditState()==TRUE ){ getTibMatCol() } else { NULL } }),
  matColIndexChoices=reactive({  if( tibEditState()==TRUE ){ getTibMatColChoices() } else { NULL } }),
  columnName= reactive({         if( tibEditState()==TRUE ){ getTibColumnName() } else { NULL } }),
  columnNameChoices=reactive({   if( tibEditState()==TRUE ){ getTibColumnNameChoices() } else { NULL } }),
  getTibEntry=reactive({         if( tibEditState()==TRUE ){ getTibEntry() } else { NULL } }),
  getTibEntryChoices=reactive({  if( tibEditState()==TRUE ){ getTibEntryChoices() } else { NULL } }),
  tibEditState=tibEditState,
  headerId=NS("tagValBar", 'header')
)


#name
observeEvent(returnValue4ModuleEdTib$name(),{
  
    name<-returnValue4ModuleEdTib$name()
    if(name==getTibName()){ return(NULL) } #bail if moduleEdTib did not change name
    if(name==transformTag){
     setPlotState('transform')
      updateSelected(name=transformTag)
    } else if(name==logTag){
        setPlotState(logTag)
        updateSelected(name=logTag)
      } 

    else {
      setPlotState(NULL)
      tibs<-getPtDefs()$tib
      resetSelectedTibbleName(tibs=tibs, name=name)
    }
})

observeEvent(returnValue4ModuleEdTib$transformType(),{
  if( getPlotState()=='transform'){
      tt<-returnValue4ModuleEdTib$transformType()
      if(!is.null(tt)){ cat('tt=',tt,'\n') } else{ cat('tt in NULL\n') }
      if(!is.null(tt) && tt!=selectedTibble$transformType){
        selectedTibble$transformType<-tt
      }
  }
})

# rowIndex
# if moduleEdTib changes the rowIndex,  matCol in selectedTibble needs to be updated
observeEvent(returnValue4ModuleEdTib$rowIndex(),{
  if( tibEditState()==TRUE ){
    #cat("returnValue4ModuleEdTib$rowIndex()\n")
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
  if( tibEditState()==TRUE ){
    matColIndex<-returnValue4ModuleEdTib$matColIndex()
    if( !is.null(matColIndex) ){ #add check for range
      updateSelected( matCol=matColIndex )
    }
  }
})

#  columnName
observeEvent(returnValue4ModuleEdTib$columnName(),{
  if( tibEditState()==TRUE ){
    colName<-returnValue4ModuleEdTib$columnName()
    if(!is.null(colName) && nchar(colName)>0 ){
      updateSelected(columnName=colName)
    }
  }
})



#--------EDIT VALUE------------------------------
observeEvent(returnValue4ModuleEdTib$entryValue(),{
  if( tibEditState()==TRUE ){
    #cat("returnValue4ModuleEdTib$entryValue()\n")
    # assuming tib is uptodate, simply work on the existing tib
    name<- returnValue4ModuleEdTib$name() 
    entry<-name %AND% returnValue4ModuleEdTib$entryValue()
 
    row= entry %AND% returnValue4ModuleEdTib$rowIndex()
    columnName<-row %AND% returnValue4ModuleEdTib$columnName()
    
    
    if(!is.null(columnName) && nchar(entry)>0   ){
      setPlotState(entry) # this is where we handle points/matrix
      if(!(entry %in% c('matrix','point'))){
        name<-getTibName()
        newPtDefs<-getPtDefs()
        column<-getTibColumnName() 
        # !!! todo: refactor
        #should be exactly the same as returnValue4ModuleEdTib$columnName()
        # so column and columnName are redundent
        row<-newPtDefs$tib[[name]] %AND% getTibRow()
        if(isNumericString(entry)){
          entry<-as.numeric(entry)
        }
        if(!is.null(row) && row>=1 && row<=nrow(newPtDefs$tib[[name]]) ){
          sender='applyTibEdit'
          newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
          updateAceExtDef(newPtDefs, sender=sender)
          updateSelected( name=name, columnName=columnName, row=row)
        }
      } 
    }
  }
})

observeEvent(
  returnValue4ModuleEdTib$tagClone(),
  {
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
)

observeEvent(
  returnValue4ModuleEdTib$tagDelete(),
  {
    sender='deleteRow'
    ptDefs<-getPtDefs()
    name<-getTibName()
    newTib<-ptDefs$tib[[name]]
    rowIndex<-getTibRow()
    # !!!TODO handle case where this would be last existing row. What to do???
    # for now we ignore
    if(is.null(newTib) || nrow(newTib)<2){ return(NULL) }
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
)

observeEvent( returnValue4ModuleEdTib$tagMoveUp(),{ 
  
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
  
})

observeEvent( returnValue4ModuleEdTib$tagMoveDown(),{ 
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

})


#-------points----------------------------------------------

#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( returnValue4ModuleEdTib$removePt(), {
  selection<-getTibName() 
  if(selection!=""){
    ptDefs<-getPtDefs()
    if(length(ptDefs$tib)==0){return(NULL)}
    matCol<-getTibMatCol()
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

  src<-getCode() 
  selection<-getTibName()
  ptDefs<-getPtDefs()
  
  row=getTibRow() 
  matCol=getTibMatCol() 
  
  m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
  if(ncol(m)<1){ 
    return(NULL) # bail if matrix of points is empty
  }
  ptDefs$mats[selection]<-FALSE # no longer a matrix input!
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
    matColIndex=max(matColIndex-1, min(matColChoices) )
    updateSelected(  matCol=matColIndex  )
  }
})

