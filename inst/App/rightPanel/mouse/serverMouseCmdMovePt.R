mouseCmdMovePt<- function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  updateRowPicker(session, "myTibRowCntrl", removeEntireGroup=TRUE)
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
  txt<-getPreProcScript()['onMovePt']
  if( !is.null(txt) ){
    tryCatch({ 
      getPoint<-function(){names(newPt)<-c('x','y'); newPt}
      context<-list(
          name=getAssetName(),
          column=getTibPtColPos(),
          row=rowIndex,
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
      if(!is.null(newPtDefs)){ #update only upon success
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx))
      }
    },error=function(e){
      e<-c('preproErr',unlist(e))
      err<-paste(unlist(e), collapse="\n", sep="\n")
      alert(err)
    })
  } else {
    newPtDefs$tib[[selection]][[ rowIndex, getTibPtColPos() ]][,matColIndx]<-newPt
    if(!is.null(newPtDefs)){ #update only upon success
      updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx))
    }
  }
}
