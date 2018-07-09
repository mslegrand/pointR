mouseCmdTransform<- function(mssg){
  sender=paste0(transformTag, '.mouse')
  vec<-mssg$vec
  tid<-mssg$id
  trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
  pos<-tid2replacementCoord(tid)
  replacementList<-list(list(rng=pos, txt= trDefDelta2))
  updateAceExt(id= getAceEditorId(), replacement=replacementList, sender = sender, ok=1 )
}
