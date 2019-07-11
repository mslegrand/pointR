
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
    #cat("serverRowDND:: -----Entering-----rowIndex()::----------------\n")
    rowIndex<-as.integer(returnValue4ModuleRowDND$rowIndex())
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
        # tryCatch({ 
        #   txt<-getPreProcPtScript()['onChangeRow']
        #   newPtDefs<-ptDefs
        #   tibs<-ptDefs$tib
        #   tib<-tibs[[selection]]
        #   values<-tib[[getTibColumnName()]]
        #   cat('here A\n')
        #   if(identical(class(values),'list')){
        #     stop('cannot edit list column')
        #   }
        #   cat('here B\n')
        #   print(values)
        #   getAttrValue<-function(){values[rowIndex]}
        #   cat(getAttrValue(),"\n")
        #   context<-list(
        #     name=getAssetName(),
        #     column=getTibColPos(),
        #     row=rowIndex,
        #     tibs=tibs
        #   )
        #   cat('here C\n')
        #   print(context)
        #   ppenv<-list(
        #     setAttrValue=setAttrValue,
        #     getAttrValue=getAttrValue,
        #     context=context,
        #     keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey)
        #   )
        #   cat('here D\n')
        #   tibs<-eval(parse(text=txt), ppenv )
        #   cat('here F\n')
        #   print(tibs$points)
        #   
        #   validateTibLists(getPtDefs()$tib, tibs)
        #   cat('here G\n')
        #   newPtDefs$tib<-tibs
        #   cat('here H\n')
        #   
        #   if(!is.null(tibs)){ #update only upon success
        #     cat('here I\n')
        #     sender='tagDrag.mouse'
        #     matCol<-ncol(tibs[[getAssetName() ]][[rowIndex, getTibPtColPos()]])
        #     sender='applyTibEdit'
        #     updateAceExtDef(newPtDefs, sender=sender, selector=list( name=context$name, rowIndex=context$row   ) )
        #   }
        # }, error=function(e){
        #   e<-c('preproErr',e)
        #   err<-paste(unlist(e), collapse="\n", sep="\n")
        #   # cat(err)
        #   alert(err)
        # })
      } else {
        updateSelected( rowIndex=rowIndex)
      }
      
      
    }
    #cat("serverEdTib:: -----Leaving-----rowIndex()::----------------\n")
  }
}, ignoreNULL = TRUE)

#row reordering
observeEvent( returnValue4ModuleRowDND$rowReorder() ,{
  if( getTibEditState()==TRUE ){
    ordering<-as.integer(returnValue4ModuleRowDND$rowReorder())
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
