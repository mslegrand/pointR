# mouse add points

#// called from mouseCmdAddPt when adding a new point
addPt2ptDefs<-function(name, row, matCol,  ptDefs, newPt){
  if(is.numeric(row) && 
     is.numeric(matCol)  &&
     row>0 && 
     length(ptDefs$tib)>0 && 
     length(ptDefs$tib[[name]])>0 && 
     row<=nrow(ptDefs$tib[[name]])
  ){
    tib<-ptDefs$tib[[name]]
    col<-getTibPtColPos() #which(names(tib)==ptColName)
    pts<-tib[[row,col]] 
    pts<-append(pts,newPt,2*(matCol))
    tib[[row,col]]<-matrix(pts,2)
    ptDefs$tib[[name]]<-tib
  } else {
    ptDefs<-NULL #failed
    #cat("addPt2ptDefs returning NULL")
  }
  ptDefs
}

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
        tibs<-eval(parse(text=txt)) # may want to restrict the env to given set of fns: addPt2ptDefs, getSVGWH
        validateTibLists(getPtDefs()$tib, tibs)
        newPtDefs$tib<-tibs
        if(!is.null(newPtDefs)){ #update only upon success
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
        }
      },error=function(e){
        e<-c('preproErr',e)
        err<-paste(unlist(e), collapse="\n", sep="\n")
        # cat(err)
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
