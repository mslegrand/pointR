
returnValue4ModuleRtFtr<-callModule(
  module=moduleFooterRight,
  id="footerRight",
  getTibEditState= tibEditState,
  getPanelState=getRightMidPanel2,
  getHandler=getHandler,
  getHandlerValue=getHandlerValue #reactive({  if( tibEditState()==TRUE ){ getHandlerValue() } else { NULL } })
)

#-----------BUTTON EVENTS--------------------


observeEvent(
  returnValue4ModuleRtFtr$tagClone(),
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
  returnValue4ModuleRtFtr$tagDelete(),
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

observeEvent( returnValue4ModuleRtFtr$tagMoveUp(),{

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

observeEvent( returnValue4ModuleRtFtr$tagMoveDown(),{
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


#---BUTTON: remove selected point  -----

observeEvent( returnValue4ModuleRtFtr$removePt(), {
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
observeEvent( returnValue4ModuleRtFtr$tagPt(), {

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
observeEvent( returnValue4ModuleRtFtr$forwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length( matColIndex)>0 && length(matColChoices)>0){
    matColIndex=min(matColIndex+1, max(matColChoices) )
    updateSelected(  matCol=matColIndex )
  }
})

# backward point
observeEvent( returnValue4ModuleRtFtr$backwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length(matColIndex)>0 && length(matColChoices)>0){
    matColIndex=max(matColIndex-1, min(matColChoices) )
    updateSelected(  matCol=matColIndex  )
  }
})

observeEvent(returnValue4ModuleRtFtr$useColourPalette(), {
  hv<-returnValue4ModuleRtFtr$useColourPalette()
  if(is.null(hv) || is.null(getHandler())){
    return(NULL)
  }
  if(hv=='FALSE'){
    hv<-NULL
  } else {
    hv<-'colourable'
    # should update the colourInput???
  }
  cat('\n\n===observeEvent(returnValue4ModuleRtFtr$useColourPalette()=== ')
  setHandlerValue(hv)
})

