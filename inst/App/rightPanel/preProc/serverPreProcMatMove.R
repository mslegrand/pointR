# user util for matrix move preprossesing
moveMatrix<-function(dxy=getDxy(), location=getLocation()  ){
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
      m<- tib[[rowIndex, getTibPtColPos() ]]
      m<-m+dxy
      tib[[rowIndex, getTibPtColPos() ]]<-tib[[rowIndex, getTibPtColPos() ]]+dxy
      tibs[[location$assetName]]<-tib
    }
  }
  tibs
}


