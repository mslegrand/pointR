mouseCmdValue<- function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs()
  tmp<-unlist(str_split(mssg$id,"_")) 
  row<-as.numeric(tail(tmp,1))

  if( mssg$shiftKey==TRUE){ #add row to rowGroupsDB
    if(getTibRow()!=row){
      updateRowPicker(session, "myTibRowCntrl", addToGroup = row, selectRow = row )
    } else {
      updateRowPicker(session, "myTibRowCntrl", toggleGroup = row)
    }
  }  else {
    updateRowPicker(session, "myTibRowCntrl", removeEntireGroup=TRUE)
  }
  sender='tagValue.mouse'
  tid<-mssg$id
  tmp<-unlist(str_split(tid,"_"))
  rowIndex<-as.numeric(tail(tmp,1))
  selection<-getAssetName()
  
  #--- insert hook here
  if( !is.null(getPreProcScript()['onChangeRow'])){
    preprocTrySetAttrValue('onChangeRow', ptDefs, rowIndex, selection, mssg)
  } else {
    updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex))
  }
}
