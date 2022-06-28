#' sets preproc value for 'onChangeRow' for 
#'   1. when row is changed by row selector in serverRowIndexCtrl.R
#'   2. when row is changed by mouse click (in value mode: tagDrag.mouse in serverMouseCmdValue.R) 
preprocTrySetAttrValue<-function( cmd.Row, ptDefs, rowIndex, selection, mssg=NULL){
  log.fin(preprocTrySetAttrValue)
  if(is.null(mssg)){
    mssg<-list(altKey=FALSE, shiftKey=FALSE, ctrlKey=FALSE, metaKey=FALSE, keycode=NULL)
  }
  tryCatch({ 
    txt<-getPreProcScript()['onChangeRow']
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
      getLastRow=getLastRow,
      replaceLastRow=replaceLastRow,
      appendLastRow=appendLastRow,
      appendAttrValues=appendAttrValues,
      context=context,
      keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey, keycode=mssg$keycode, char=mssg$char)
    )
    tibs<-eval(parse(text=txt), ppenv )
    validateTibLists(getPtDefs()$tib, tibs)
    ptDefs$tib<-tibs
    sender='applyTibEdit'
    updateAceExtDef(ptDefs, sender=sender, selector=list( name=context$name, rowIndex=context$row   ) )
  }, error=function(e){
    e<-c('onChangeRow:',e$message)
    err<-paste(unlist(e), collapse="\n", sep="\n")
    shinyalert("preproc value Errpr",err, type="error")
  })
  log.fin(preprocTrySetAttrValue)
}

#' Sets preproc values for
#'  returnValue4ModuleRtFtr$tagPt() in serverFooterRight.R
#'  This differes from  preprocTrySetAttrValue by interating over all columns
#'  and not just a single column
preprocTrySetAttrValueS<-function(scripts,  ptDefs, rowIndex, selection){
  row<-rowIndex
  tryCatch({
    tibs<-ptDefs$tib
    tib<-tibs[[selection]]
    tibColNames<-names(tib)
    if(length(scripts)>0){
      cols<-intersect(tibColNames,names(scripts))
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
          getLastRow=getLastRow,
          replaceLastRow=replaceLastRow,
          appendLastRow=appendLastRow,
          appendAttrValues=appendAttrValues,
          context=context,
          #keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey, mssg$keycode)
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey, keycode=mssg$keycode, char=mssg$char)
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
    e<-c('preprocErr',e$message)
    err<-paste(unlist(e), collapse="\n", sep="\n")
    shinyalert("preproc value Errpr",err, type="error")
  })
}
  