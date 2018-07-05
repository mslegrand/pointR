mouseCmdMovePt<- function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  
  cat('Enter: mouse cmd move')
  sender='PointsBar.mouse.move'
  id<-mssg$id
  newPtDefs<-ptDefs
  vid<-strsplit(id,"-")[[1]] 
  #get selection
  selection<-vid[2]
  #get point index
  newPt<-vec
  rowIndex<-as.numeric(vid[3]) # index is the absolute position of in the points array
  matColIndx<-as.numeric(vid[4])
  if( hasPtScript() ){
    cat('hasPtScript:: onMovePt script:\n')
    txt<-getPreProcPtScript()['onMovePt']
    cat(txt)
    tryCatch({ 
      getPoint<-function(){newPt}
      getLocation<-function(){
        list(
          assetName=getAssetName(),
          columIndex=getTibPtColPos(),
          rowIndex=rowIndex,
          matColIndex=matColIndx,
          tibs=getPtDefs()$tib
        )
      }
      tibs<-eval(parse(text=txt))
      newPtDefs$tib<-tibs
    },error=function(e){
      e<-c('preproErr',unlist(e))
      err<-paste(unlist(e), collapse="\n", sep="\n")
      setErrorMssg(err)
    })
  } else {
    newPtDefs$tib[[selection]][[ rowIndex, getTibPtColPos() ]][,matColIndx]<-newPt
  }
  updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx))
}
