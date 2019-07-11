mouseCmdValue<- function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  
  sender='tagDrag.mouse'
  tid<-mssg$id
  tmp<-unlist(str_split(tid,"_"))
  rowIndex<-as.numeric(tail(tmp,1))
  selection<-getAssetName()
  
  #--- insert hook here
  if(hasPtScript() && !is.null(getPreProcPtScript()['onChangeRow'])){
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
    #   if(!is.null(newPtDefs)){ #update only upon success
    #     cat('here I\n')
    #     #sender='cmd.commit'
    #     # sender='cmd.valueChange'
    #     # updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex ))
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
    # m<-ptDefs$tib[[selection]][[ rowIndex, getTibPtColPos() ]]
  # matCol<-ncol(m) #IS THIS NECESSARY???
  
  #updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matCol))
  updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex))
  }
  
  #---
  
  
}
