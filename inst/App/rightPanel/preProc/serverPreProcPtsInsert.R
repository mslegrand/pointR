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
      pts<-tib[[columnIndex]][[rowIndex]]
      pts<-append(pts,pt,2*(matColIndex))
      tib[[columnIndex]][[rowIndex]]<-matrix(pts,2)
      tibs[[assetName]]<-tib
    }
  }
  tibs
}


