# mouse add points
mouseCmdAddPt<-function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  tibs<-getPtDefs()$tib
  sender='PointsBar.mouse.add'
  
  
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
    } 
  }
  #{
    # now  add
    newPtDefs<-ptDefs
    if( hasPtScript() ){ #preproc
      txt<-getPreProcPtScript()['onNewPt']
      tryCatch({
        getPoint<-function(){names(newPt)<-c('x','y'); newPt}
        context<-list(
          name=getAssetName(),
          column=getTibPtColPos(),
          row=rowIndex,
          ptIndex=matColIndx,
          tibs=tibs
        )
        ppenv<-list(
          getPoint=getPoint,
          context=context,
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey),
          WH=getSVGWH()
        )
        tibs<-eval(parse(text=txt), ppenv )
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
      if(!is.null(newPtDefs)){ #update only upon success
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
      }    
    }
  #} #end no split
  
}
