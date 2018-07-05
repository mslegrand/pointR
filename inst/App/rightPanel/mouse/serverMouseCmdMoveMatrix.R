mouseCmdMoveMatrix<-function(mssg){
  cat('\n---------------Entering---mouseCmdMoveMatrix------------------------\n')
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  
  sender='tagDrag.mouse'
  tid<-mssg$id
  dxy<-vec 
  tmp<-unlist(str_split(tid,"_"))
  row<-as.numeric(tail(tmp,1))
  
  selection<-getAssetName() 
  m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
  ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m+vec
  matCol<-ncol(m)
  newPtDefs<-ptDefs
  updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))  
}