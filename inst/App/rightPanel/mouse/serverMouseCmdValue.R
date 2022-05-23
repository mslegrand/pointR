mouseCmdValue<- function(mssg){
  #cat('mouseCmdVal\n')
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs()
  tmp<-unlist(str_split(mssg$id,"_")) 
  row<-as.numeric(tail(tmp,1))
  mssg$char<-NULL
  if (length(mssg$keycode)>0){
    kc<-mssg$keycode
    if(  (65<=kc && kc<=90 ) || (40<=kc && kc<=57)){ #process char or numeric only 
      if(mssg$shiftKey==FALSE && 65<=kc && kc<=90){
        kc=kc+32
      } 
      mode(kc)<-'raw'
      kc<-rawToChar(kc)
      mssg$char<-kc
    }
  } 
   
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
