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
  tibs<-getPtDefs()$tib
  
  sender='PointsBar.mouse.add'
  # browser()
  
  newPt<-vec
  selection<-getAssetName() 
  rowIndex<-getTibRow()
  matColIndx<-getTibMatCol()
  if( length( getPointMax())>1){ stop('getPointMax is too big')} #should never happen
  if(!is.na(getPointMax()) && getTibMatColMax() >= getPointMax() ){
    if(matColIndx<getPointMax() ){ # matcol not at end and ptmax for this row is already exceeded
      # eat this point
    } else {  #split
        tib<-tibs[[selection]]
        tib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
        rowIndex<-rowIndex+1
        tib[[getTibColumnName()]][[rowIndex]]<-matrix(0,2,0)
        tibs[[selection]]<-tib
        matColIndx<-0
        # updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=1))
    } 
  }
  #{
    # now  add
    newPtDefs<-ptDefs
    if( hasPtScript() ){ #preproc
      txt<-getPreProcPtScript()['onNewPt']
      tryCatch({
        getPoint<-function(){names(newPt)<-c('x','y'); newPt}
        WH<-getSVGWH()
        getLocation<-function(){
          list(
            assetName=selection,
            columIndex=getTibPtColPos(),
            rowIndex=rowIndex,
            matColIndex=matColIndx,
            tibs=tibs
          )
        }
        tibs<-eval(parse(text=txt), list() ) # may want to restrict the env to given set of fns: addPt2ptDefs, getSVGWH
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
      tib<-tibs[[selection]]
      pts<-tib[[getTibColumnName()]][[rowIndex]]
      pts<-matrix(append(pts,newPt,2*(matColIndx)) ,2)
      tibs[[selection]][[getTibColumnName()]][[rowIndex]]<-pts
      newPtDefs$tib<-tibs
      # newPtDefs<-addPt2ptDefs(
      #   selection,
      #   rowIndex,
      #   matColIndx,
      #   ptDefs, 
      #   newPt 
      # )
      if(!is.null(newPtDefs)){ #update only upon success
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
      }    
    }
  #} #end no split
  
}
