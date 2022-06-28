# user util for matrix move preprossesing
moveMatrix<-function(dxy=getDxy(), context=context  ){
  tibs<-context$tibs
  assetName<-context$name
  if( !is.null(tibs) &&
      is_scalar_character(assetName) && 
      assetName %in% names(tibs) 
  ){
    tib<-tibs[[ assetName ]]
    columnIndex<-context$column
    
    rowIndex<-context$row
    
    if(  is_scalar_numeric(rowIndex) &&
      rowIndex>0 && 
      rowIndex<=nrow(tib) &&
      is_scalar_numeric(ncol( tib[[columnIndex]][[rowIndex]]))
    ){
      # m<- tib[[columnIndex]][[rowIndex]]
      # m<-m+dxy
      tib[[columnIndex]][[rowIndex]]<-tib[[columnIndex]][[rowIndex]]+dxy
    } 
    
    tibs[[assetName]]<-tib
  }
  tibs
}


