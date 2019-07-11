#' sets preproc value for 'onChangeRow' for 
#'   1. when row is changed by row selector in serverRowDND.R
#'   2. when row is changed by mouse click (in value mode: tagDrag.mouse in serverMouseCmdValue.R) 
preprocTrySetAttrValue<-function( cmd.Row, ptDefs, rowIndex, selection){

  tryCatch({ 
    txt<-getPreProcPtScript()['onChangeRow']
    tibs<-ptDefs$tib
    tib<-tibs[[selection]]
    values<-tib[[getTibColumnName()]]
    if(identical(class(values),'list')){
      stop('cannot edit list column')
    }
    getAttrValue<-function(){values[rowIndex]}
    context<-list(
      name=getAssetName(),
      column=getTibColPos(),
      row=rowIndex,
      tibs=tibs
    )
    ppenv<-list(
      setAttrValue=setAttrValue,
      getAttrValue=getAttrValue,
      context=context,
      keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey)
    )
    tibs<-eval(parse(text=txt), ppenv )
    validateTibLists(getPtDefs()$tib, tibs)
    ptDefs$tib<-tibs
    sender='applyTibEdit'
    updateAceExtDef(ptDefs, sender=sender, selector=list( name=context$name, rowIndex=context$row   ) )
  }, error=function(e){
    e<-c('preproErr',e)
    err<-paste(unlist(e), collapse="\n", sep="\n")
    alert(err)
  })
}

#' Sets preproc values for
#'  returnValue4ModuleRtFtr$tagPt() in serverFooterRight.R
#'   
preprocTrySetAttrValueS<-function(scripts,  ptDefs, rowIndex, selection){
  row<-rowIndex
  tryCatch({
    tibs<-ptDefs$tib
    tib<-tibs[[selection]]
    tibColNames<-names(tib)
    if(length(scripts)>0){
      cols<-names(scripts)
      for(columnName in cols){
        txt<-scripts[columnName]
        values<-tib[[columnName]]
        getAttrValue<-function(){values[rowIndex]}
        context<-list(
          name=getAssetName(),
          column=which(tibColNames==columnName),
          row=rowIndex,
          tibs=tibs
        )
        ppenv<-list(
          setAttrValue=setAttrValue,
          getAttrValue=getAttrValue,
          context=context,
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey)
        )
        tibs<-eval(parse(text=txt), ppenv )
        validateTibLists(getPtDefs()$tib, tibs)
      }
      if(!is.null(tibs)){
        ptDefs$tib<-tibs
        sender='tagPt'
        updateAceExtDef(ptDefs, sender=sender, selector=list( name=context$name, rowIndex=context$row   ) )
      }
    }
  }, error=function(e){
    e<-c('preproErr',e)
    err<-paste(unlist(e), collapse="\n", sep="\n")
    alert(err)
  })
}
  