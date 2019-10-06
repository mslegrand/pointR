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
    trow<-tib[nrow(tib),]
    trow[,names(vals)]<-vals
    tib<-bind_rows(tib, trow)
  }
  tib
}

getLastRow<-function(tib){
  tib[nrow(tib),]
}
replaceLastRow<-function(tib, row){
  tib[nrow(tib),]<-row
  tib
}

appendLastRow<-function(tib, row){
  rbind(tib,row)
}

