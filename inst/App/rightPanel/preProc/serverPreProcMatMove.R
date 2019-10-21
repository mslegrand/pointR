# user util for matrix move preprossesing
moveMatrix<-function(dxy=getDxy(), context=context  ){
  tibs<-context$tibs
  assetName<-context$name
  if( !is.null(tibs) &&
      is_scalar_character(assetName) && 
      assetName %in% names(tibs) 
  ){
    tib<-tibs[[ assetName ]]
    rowIndex<-   context$row
    columnIndex<-context$column
    matColIndex<-context$ptIndex
    if( 
      is_scalar_numeric(rowIndex) &&
      is_scalar_numeric(matColIndex)  &&
      rowIndex>0 && 
      rowIndex<=nrow(tib)
    ){
      m<- tib[[rowIndex, columnIndex ]]
      m<-m+dxy
      tib[[rowIndex, columnIndex ]]<-tib[[rowIndex, columnIndex ]]+dxy
      tibs[[assetName]]<-tib
    }
  }
  tibs
}


