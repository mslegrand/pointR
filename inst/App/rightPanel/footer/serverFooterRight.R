
returnValue4ModuleRtFtr<-callModule(
  module=moduleFooterRight,
  id="footerRight",
  getTibEditState= getTibEditState,
  getPointMax=reactive({
    val<-getPointMax()
    if(is.na(val)){ val<-NULL}
    val
  }),
  getPanelState=reactive({
    rtv<-getRightMidPanel(); 
    rtv
  }),
  hasPreProcChoices=hasPreProcChoices ,
  getScriptName=reactive({
    getPreProcScriptName(tab_Id=getTibTabId(), tib_Name=getAssetName(),column_Name= getTibColumnName())
  })
)

#-----------BUTTON EVENTS--------------------


observeEvent(
  returnValue4ModuleRtFtr$tagClone(),
  {
    sender='cloneRow'
    ptDefs<-getPtDefs()
    selection<-getAssetName()
    tib<-ptDefs$tib[[selection]]
    rowIndex<-getTibRow()
    updateRowPicker(session, "myTibRowCntrl", insertRow=rowIndex+1, selectRow=rowIndex+1)
    newTib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
    rowIndex=rowIndex+1
    matCol<-ncol(newTib[[ getTibPtColPos()]][[rowIndex]])
    pts<-newTib[[getTibPtColPos()]] #!!!!  NOT USED?????
    ptDefs$tib[[selection]]<-newTib
    tabId<-getTibTabId()
    scripts<-getPreProcOnNewRowScripts(tabId, selection)
    if(length(scripts)>0){
      preprocTrySetAttrValueS(scripts,  ptDefs, rowIndex, selection)
    } else {
      updateAceExtDef(ptDefs, sender=sender, selector=list(rowIndex=rowIndex, matCol=matCol  ) )
    }
  }
)

observeEvent(
  returnValue4ModuleRtFtr$tagDelete(),
  {
    sender='deleteRow'
    ptDefs<-getPtDefs()
    name<-getAssetName()
    newTib<-ptDefs$tib[[name]]
    rowIndex<-getTibRow()
    updateRowPicker(session, "myTibRowCntrl", deleteRow=rowIndex)
    # !!!TODO handle case where this would be last existing row. What to do???
    # for now we ignore
    if(is.null(newTib) || nrow(newTib)<2){ return(NULL) }
    newTib<-newTib[-rowIndex,]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs

    #adjust position
    rowIndex<-min(rowIndex, nrow(newTib))
    matCol<-ncol(newTib[[getTibPtColPos()]][[rowIndex]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matCol   ) )
  }
)

observeEvent( returnValue4ModuleRtFtr$tagMoveUp(),{

  rowIndex<-getTibRow()
  if(rowIndex>1){
    sender='tagMoveUp'
    ptDefs<-getPtDefs()
    name<-    getAssetName()
    newTib<-ptDefs$tib[[name]]

    newTib[c(rowIndex,rowIndex-1),]<-newTib[c(rowIndex-1,rowIndex),]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs

    #adjust position
    rowIndex<-rowIndex-1
    matCol<-ncol(newTib[[ getTibPtColPos()]][[rowIndex]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matCol   ) )
  }

})

observeEvent( returnValue4ModuleRtFtr$tagMoveDown(),{
  rowIndex<-getTibRow()
  ptDefs<-getPtDefs()
  name<-    getAssetName()
  newTib<-ptDefs$tib[[name]]
  if(rowIndex<nrow(newTib)){
    sender='tagMoveDown'

    newTib[c(rowIndex,rowIndex+1),]<-newTib[c(rowIndex+1,rowIndex),]
    ptDefs$tib[[name]]<-newTib
    newPtDefs<-ptDefs

    #adjust position
    rowIndex<-rowIndex+1
    matCol<-ncol(newTib[[getTibPtColPos()]][[rowIndex]])
    if(length(matCol)==0){matCol=0}
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matCol   ) )
  }

})


#---BUTTON: remove selected point  -----

observeEvent( returnValue4ModuleRtFtr$removePt(), {
  selection<-getAssetName()
  if(selection!=""){
    ptDefs<-getPtDefs()
    if(length(ptDefs$tib)==0){return(NULL)}
    matCol<-getTibMatCol()
    #get row, col
    if(matCol>=1){
      row<-getTibRow()
      m<-matrix(ptDefs$tib[[selection]][[getTibPtColPos() ]][[row]] [,-matCol],2)
      #!!! probably need some checking here
      ptDefs$tib[[selection]] [[getTibPtColPos() ]][[ row]]<-m
      matCol<-min(matCol, length(m)/2)
      newPtDefs<-ptDefs
      sender='points.deletePoint'
      updateAceExtDef(newPtDefs, sender=sender, selector=list( matCol=matCol )   )
    }
  }
}) #end remove point observer

#----begin for Tagging-------------------------------------



#---TAG THIS POINT button-----
observeEvent( returnValue4ModuleRtFtr$tagPt(), {

  # src<-getCode() #why
  sender='tagPt'
  ptDefs<-getPtDefs()
  selection<-getAssetName()
  tib<-ptDefs$tib[[selection]] #get the tib
  rowIndex<-getTibRow()
  matCol<-getTibMatCol()
  
  updateRowPicker(session, "myTibRowCntrl", insertRow=rowIndex+1, selectRow=rowIndex+1)
  
  m<-tib[[getTibPtColPos() ]][[ rowIndex]]
  ptDefs$mats[selection]<-FALSE # no longer a matrix input!
  newTib<-tagTib(tib, getTibPtColPos(), rowIndex, matCol)
  
  
  rowIndex<-rowIndex+1
  matCol<-length(newTib[[getTibPtColPos()]][[rowIndex]])/2
  ptDefs$tib[[selection]]<-newTib
  
  tabId<-getTibTabId()
  scripts<-getPreProcOnNewRowScripts(tabId, selection)
  
  if(length(scripts)>0){
    preprocTrySetAttrValueS(scripts,  ptDefs, rowIndex, selection)
  } else {
    updateAceExtDef(ptDefs, sender=sender, selector=list(rowIndex=rowIndex, matCol=matCol   ) )
  }
}) 

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

observeEvent( returnValue4ModuleRtFtr$matColLim(), {
  mcl<-returnValue4ModuleRtFtr$matColLim()
  
  curVal<-getPointMax() 
  
  if( !( identical(mcl,  curVal ))){
      updateWidgetChoicesRow(maxVal= mcl )
   }
})

observeEvent( returnValue4ModuleRtFtr$tagSetValue(),{
  aName<-getAssetName()
  cname<-getTibColumnName()
  newPtDefs<-getPtDefs()
  # get the current value of the selectedRowIndex and selectedColumn
  rowIndx<-getTibRow()
  value<-newPtDefs$tib[[aName]][[cname]][[rowIndx]]
  # get the group rows
  group<-filter(rowGroupsDB(), tabId==getTibTabId(),  name==aName, colName==cname)$rows
  # for each row in the group, set the values of the selected column to the current value
  #browser()
  for(row in group){
    newPtDefs$tib[[aName]][[cname]][[row]]<-value
  }
  # update ace
  sender<-'setTibValue'
  updateAceExtDef(newPtDefs, sender=sender )
})

