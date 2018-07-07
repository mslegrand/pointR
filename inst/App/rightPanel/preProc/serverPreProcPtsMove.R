# user util for point move preprossesing
movePoint<-function(pt, location=getLocation()  ){
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
      columIndex<-getTibPtColPos() #ie. which(names(tib)==ptColName)
      tib[[rowIndex,columIndex]][,location$matColIndex] <-pt
      tibs[[location$assetName]]<-tib
    }
  }
  tibs
}


