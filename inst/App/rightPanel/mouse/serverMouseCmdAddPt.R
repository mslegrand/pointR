# mouse add points
mouseCmdAddPt<-function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  ptDefs<-getPtDefs() 
  updateRowPicker(session, "myTibRowCntrl", removeEntireGroup=TRUE)
  
  sender='PointsBar.mouse.add'
  
  
  newPt<-vec
  
  selection<-getAssetName() 
  rowIndex<-getTibRow()
  matColIndx<-getTibMatCol()
  
  
  if( length( getPointMax())>1){ stop('getPointMax is too big')} #should never happen

  if(!is.na(getPointMax()) && getTibMatColMax() >= getPointMax() ){ #need to split?
      #split
      updateRowPicker(session, "myTibRowCntrl", insertRow=rowIndex+1, selectRow=rowIndex+1)
      tibs<-ptDefs$tib
      tib<-tibs[[selection]]      
      tib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
      rowIndex<-rowIndex+1
      tib[[getTibColumnName()]][[rowIndex]]<-matrix(0,2,0)
      tibs[[selection]]<-tib
      matColIndx<-0
      ptDefs$tib<-tibs
      # since we just added a new row we must check if we need to
      # modify (preproc) the values in that row
      
      
      scripts<-getPreProcOnNewRowScripts( getTibTabId(), selection)
      if(length(scripts)>0){
          newTibs<-tibs # backup tibs, 
          newRowIndx<-rowIndex
          tryCatch({
            tibColNames<-names(tib)
            cols<-intersect(tibColNames,names(scripts))
            for(columnName in cols){
                txt<-scripts[columnName]
                values<-tib[[columnName]]
                getAttrValue<-function(){values[rowIndex]}
                context<-list(
                  name=getAssetName(),
                  column=which(tibColNames==columnName),
                  row=rowIndex,
                  tibs=tibs
                )
                ppenv<-list(
                  setAttrValue=setAttrValue,
                  getAttrValue=getAttrValue,
                  context=context,
                  keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey)
                )
                tibs<-eval(parse(text=txt), ppenv )
                validateTibLists(getPtDefs()$tib, tibs)
            } # all cols done  successfully
            ptDefs$tib<-tibs # success, reset ptDefs
          }, error=function(e){
            e<-c('preproErr',e)
            err<-paste(unlist(e), collapse="\n", sep="\n")
            alert(err)
          })
      } #end of scripts
  } # end of split
  
  # now  add  the point
  newPtDefs<-ptDefs
  tibs<-newPtDefs$tib
  txt<-getPreProcScript()['onNewPt']
  if( !is.null(txt) ){ #preproc pts 
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
          insertPoint=insertPoint,
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
        alert(err)
      })
  } else { #no prepoc pts
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
