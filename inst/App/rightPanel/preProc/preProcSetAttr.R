#set preproc value

setAttrValue<-function( value, context){
 # cat('new value',value,'\n')
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

appendAttrValues<-function(tib, missing=TRUE, ...){
  if(missing || nrow(tib)==0){
    tib<-add_row(tib, ...)
  } else{
    vals<-list(...)
    trow<-top_n(tib,1)
    trow[,names(vals)]<-vals
    tib<-bind_rows(tib, trow)
  }
  tib
}

getRowTib<-function(context){
  assetName<-context$name
  tibs<-context$tibs
  if( !is.null(tibs) &&
      is_scalar_character(assetName) && 
      assetName %in% names(tibs) 
  ){
    tib<-tibs[[ assetName ]]
    rowIndex<-   max(nrow(tib),context$row)
    return( tib[rowIndex,])
  }
}

setTibRow<-function(context, replacmentRow){
  assetName<-context$name
  tibs<-context$tibs
  if( !is.null(tibs) &&
      is_scalar_character(assetName) && 
      assetName %in% names(tibs) 
  ){
    tib<-tibs[[ assetName ]]
    rowIndex<-   max(nrow(tib),context$row)
    if(rowIndex>0 && 
       All( names(replacmentRow) %in% names(tib))
    ){
      tib[rowIndex,names(replacmentRow)]=replacementRow
      context$tibs[[asetName]]<-tib
    }
  }
}