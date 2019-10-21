# user util for point insertion preprossesing
insertPoint<-function(pt, context=context  ){
  
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
      pts<-tib[[rowIndex,columnIndex]] 
      pts<-append(pts,pt,2*(matColIndex))
      tib[[rowIndex,columnIndex]]<-matrix(pts,2)
      tibs[[assetName]]<-tib
    }
  }
  tibs
}


