# user util for point move preprossesing
movePoint<-function(pt, context=context  ){
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
      #columIndex<-getTibPtColPos() #ie. which(names(tib)==ptColName)
      tib[[rowIndex,columnIndex]][,matColIndex] <-pt
      tibs[[assetName]]<-tib
    }
  }
  tibs
}


