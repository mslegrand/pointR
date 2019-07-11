#set preproc value

setAttrValue<-function( value, context){
  cat('new value',value,'\n')
  tibs<-context$tibs
  assetName<-context$name
  if( !is.null(tibs) &&
      is_scalar_character(assetName) && 
      assetName %in% names(tibs) 
  ){
    tib<-tibs[[ assetName ]]
    rowIndex<-   context$row
    columnIndex<-context$column
    tib[[rowIndex,columnIndex]]<-value
    tibs[[ assetName ]]<-tib
  }
  return(tibs)
}