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
  } else {
    updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex))
  }
  
  #---
  
  
}
