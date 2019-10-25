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
    for(rowIndex in context$row){
      if(  is_scalar_numeric(rowIndex) &&
        rowIndex>0 && 
        rowIndex<=nrow(tib) &&
        is_scalar_numeric(ncol( tib[[ rowIndex, columnIndex]]))
      ){
        m<- tib[[rowIndex, columnIndex ]]
        m<-m+dxy
        tib[[rowIndex, columnIndex ]]<-tib[[rowIndex, columnIndex ]]+dxy
       
      } 
    }
    tibs[[assetName]]<-tib
  }
  tibs
}


