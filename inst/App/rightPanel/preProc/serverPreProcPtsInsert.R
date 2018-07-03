

getLocation<-reactive({
  list(
    assetName=getAssetName(),
    columIndex=getTibPtColPos(),
    rowIndex=getTibRow(),
    matColIndex=getTibMatCol(),
    tibs=ptDefs$tib
  )
})



insertPoint<-function(pt, location=getLocation() ){
  tibs<-ptDefs$tib
  if(length(tibs)>0 && 
     is_scalar_character(location$name) && 
     location$name %in% names(tib) &&
     is_scalar_numeric(location$rowIndex) &&
     is_scalar_numeric(location$matColIndex)  &&
     location$rowIndex>0 && 
     length(tibs[[location$assetName]])>0 && 
     row<=nrow(tibs[[location$assetName]])
  ){
      tib<-tibs[[location$assetName]]
      col<-getTibPtColPos() #which(names(tib)==ptColName)
      pts<-tib[[location$rowIndex,location$columIndex]] 
      pts<-append(pts,newPt,2*(matCol))
      tib[[rowIndex,columIndex]]<-matrix(pts,2)
      tibs[[location$assetName]]<-tib
  }
  tibs
}


