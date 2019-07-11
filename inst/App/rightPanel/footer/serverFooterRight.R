
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
    # cat('>----> getPanelState\n'); 
    rtv<-getRightMidPanel(); 
    # cat( "getPanelState= ", getRightMidPanel(),"\n" );
    # cat('<----< getPanelState\n');
    rtv
  }),
  hasPtScript=hasPtScript 
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
    newTib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
    rowIndx=rowIndex+1
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    pts<-newTib[[getTibPtColPos()]]
    ptDefs$tib[[selection]]<-newTib
    tabId<-getTibTabId()
    scripts<-getPreProcOnNewRowScripts(tabId, selection)
    if(length(scripts)>0){
      preprocTrySetAttrValueS(scripts,  ptDefs, rowIndx, selection)
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
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
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
    matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
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
      m<-matrix(ptDefs$tib[[selection]][[ row, getTibPtColPos() ]][,-matCol],2)
      #!!! probably need some checking here
      ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m
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

  src<-getCode()
  selection<-getAssetName()
  ptDefs<-getPtDefs()
  
  row<-getTibRow()
  matCol<-getTibMatCol()

  m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
  # if(ncol(m)<1){
  #   return(NULL) # bail if matrix of points is empty
  # }
  ptDefs$mats[selection]<-FALSE # no longer a matrix input!
  tib<-ptDefs$tib[[selection]] #get the tib
  tib<-tagTib(tib, getTibPtColPos(), row, matCol)
  row<-row+1
  matCol<-length(tib[[row, getTibPtColPos()]])/2
  ptDefs$tib[[selection]]<-tib
  # insert preprocValue here?????
  sender='tagPt'
  tabId<-getTibTabId()
  scripts<-getPreProcOnNewRowScripts(tabId, selection)
  if(length(scripts)>0){
    preprocTrySetAttrValueS(scripts,  ptDefs, row, selection)
     # 
     # rowIndex<-row
     # tryCatch({
     #   tibs<-ptDefs$tib
     #   tib<-tibs[[selection]]
     #   tibColNames<-names(tib)
     #   if(length(scripts)>0){
     #       cols<-names(scripts)
     #       for(columnName in cols){
     #         txt<-scripts[columnName]
     #         values<-tib[[columnName]]
     #         getAttrValue<-function(){values[rowIndex]}
     #         context<-list(
     #           name=getAssetName(),
     #           column=which(tibColNames==columnName),
     #           row=rowIndex,
     #           tibs=tibs
     #         )
     #         ppenv<-list(
     #           setAttrValue=setAttrValue,
     #           getAttrValue=getAttrValue,
     #           context=context,
     #           keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey)
     #         )
     #         tibs<-eval(parse(text=txt), ppenv )
     #         validateTibLists(getPtDefs()$tib, tibs)
     #       }
     #      
     #         if(!is.null(tibs)){
     #         ptDefs$tib<-tibs
     #         sender='tagPt'
     #         updateAceExtDef(ptDefs, sender=sender, selector=list( name=context$name, rowIndex=context$row   ) )
     #       }
     #    }
     # }, error=function(e){
     #   e<-c('preproErr',e)
     #   err<-paste(unlist(e), collapse="\n", sep="\n")
     #   # cat(err)
     #   alert(err)
     # })
  } else {
    sender='tagPt'
    updateAceExtDef(ptDefs, sender=sender, selector=list(rowIndex=row, matCol=matCol   ) )
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



