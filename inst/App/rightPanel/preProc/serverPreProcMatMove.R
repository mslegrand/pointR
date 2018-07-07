

moveMatrix<-function(dxy=getDxy(), location=getLocation()  ){
  tibs<-location$tibs
  cat('aaa\n')
  if( !is.null(tibs) &&
      is_scalar_character(location$assetName) && 
      location$assetName %in% names(tibs) 
  ){
    cat('bbb\n')
    tib<-tibs[[ location$assetName ]]
    cat('ccc\n')
    rowIndex<-location$rowIndex
    if( 
      is_scalar_numeric(location$rowIndex) &&
      is_scalar_numeric(location$matColIndex)  &&
      rowIndex>0 && 
      rowIndex<=nrow(tib)
    ){
      cat('ddd\n')
      columIndex<-getTibPtColPos() #which(names(tib)==ptColName)
      cat('rrr\n')
      m<- tib[[rowIndex, getTibPtColPos() ]]
      cat('class(m)=',class(m),"\n")
      cat('class(dxy)=',class(dxy),"\n")
      m<-m+dxy
      tib[[rowIndex, getTibPtColPos() ]]<-tib[[rowIndex, getTibPtColPos() ]]+dxy
      cat('eee\n')
      tibs[[location$assetName]]<-tib
      cat('fff\n')
    }
  }
  tibs
}


