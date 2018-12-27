mouseCmdValue<- function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  
  # cat('Enter: mouse cmd value')

  sender='tagDrag.mouse'
  tid<-mssg$id
  tmp<-unlist(str_split(tid,"_"))
  row<-as.numeric(tail(tmp,1))
  selection<-getAssetName() 
  
  m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
  matCol<-ncol(m)
  
  updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))

}
