

getLocation<-reactive({
  list(
    assetName=getAssetName(),
    columIndex=getTibPtColPos(),
    rowIndex=getTibRow(),
    matColIndex=getTibMatCol(),
    tibs=getPtDefs()$tib
  )
})



insertPoint<-function(pt, location=getLocation()  ){
  tibs<-location$tibs
  
  if( !is.null(tibs) &&
     is_scalar_character(location$assetName) && 
     location$assetName %in% names(tibs) 
  ){
   
    tib<-tibs[[ location$assetName ]]
    rowIndex<-location$rowIndex
    if( 
       is_scalar_numeric(location$rowIndex) &&
       is_scalar_numeric(location$matColIndex)  &&
       rowIndex>0 && 
       rowIndex<=nrow(tib)
    ){
      
      columIndex<-getTibPtColPos() #which(names(tib)==ptColName)
      pts<-tib[[location$rowIndex,location$columIndex]] 
      pts<-append(pts,pt,2*(location$matColIndex))
      tib[[rowIndex,columIndex]]<-matrix(pts,2)
      tibs[[location$assetName]]<-tib
      
    }
  }
  tibs
}


