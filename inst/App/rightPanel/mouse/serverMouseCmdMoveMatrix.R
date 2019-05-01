mouseCmdMoveMatrix<-function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  newPtDefs<-getPtDefs() 
  
  sender='tagDrag.mouse'
  id<-mssg$id
  dxy<-vec 
  tmp<-unlist(str_split(id,"_"))
  row<-as.numeric(tail(tmp,1)) 
  selection<-getAssetName() 
  matColIndx<-ncol(newPtDefs$tib[[selection]][[ row, getTibPtColPos() ]])
  
  if( hasPtScript() ){
      # cat('hasPtScript:: onMoveMat script:\n')
      txt<-getPreProcPtScript()['onMoveMat']
      # cat(txt)
      tryCatch({ 
        getDxy<-function(){names(dxy)<-c('dx','dy'); dxy}
        context<-list(
          name=getAssetName(),
          column=getTibPtColPos(),
          row=row,
          ptIndex=matColIndx,
          tibs=getPtDefs()$tib
        )
        ppenv<-list(
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey),
          WH=getSVGWH()
        )
        tibs<-eval(parse(text=txt), ppenv )
        validateTibLists(getPtDefs()$tib, tibs)
        newPtDefs$tib<-tibs
        matCol<-ncol(tibs[[getAssetName()]][row, getTibPtColPos()] )
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))  
      },error=function(e){
        e<-c('preproErr',unlist(e))
        err<-paste(unlist(e), collapse="\n", sep="\n")
        alert(err)
      })
  } else {
    m<-newPtDefs$tib[[selection]][[ row, getTibPtColPos() ]]
    newPtDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m+vec
    matCol<-ncol(m)
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol)) 
  }
  
  
  
   
}