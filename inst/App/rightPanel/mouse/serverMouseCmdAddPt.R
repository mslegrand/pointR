mouseCmdAddPt<-function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  sender='PointsBar.mouse.add'
  
  #cat('Enter: mouse cmd add')
  newPt<-vec
  selection<-getAssetName() 
  rowIndex<-getTibRow()
  # cat('mouseMssg:: rowIndex=',format(rowIndex),"\n")
  matColIndx<-getTibMatCol()
  if( length( getPointMax())>1){ stop('getPointMax is too big')}
  if(!is.na(getPointMax()) &&  matColIndx>=getPointMax() ){
    # #split here
    tib<-ptDefs$tib[[selection]]
    tib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
    rowIndex<-rowIndex+1
    tib[[getTibColumnName()]][[rowIndex]]<-matrix(newPt,2)
    ptDefs$tib[[selection]]<-tib
    updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=1))
  } else {
    # no split, jjust add
    newPtDefs<-ptDefs
    if( hasPtScript() ){ #preproc
      txt<-getPreProcPtScript()['onNewPt']
      tryCatch({
        getPoint<-function(){newPt}
        getLocation<-function(){
          list(
            assetName=getAssetName(),
            columIndex=getTibPtColPos(),
            rowIndex=getTibRow(),
            matColIndex=getTibMatCol(),
            tibs=getPtDefs()$tib
          )
        }
        tibs<-eval(parse(text=txt))
        validateTibLists(getPtDefs()$tib, tibs)
        newPtDefs$tib<-tibs
        if(!is.null(newPtDefs)){ #update only upon success
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
        }
      },error=function(e){
        e<-c('preproErr',e)
        err<-paste(unlist(e), collapse="\n", sep="\n")
        cat(err)
        alert(err)
      })
    } else { #no prepoc
      newPtDefs<-addPt2ptDefs(
        selection,
        rowIndex,
        matColIndx,
        ptDefs, 
        newPt 
      )
      if(!is.null(newPtDefs)){ #update only upon success
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
      }    
    }
  } #end no split
  
}
