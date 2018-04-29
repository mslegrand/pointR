library(tidyverse)

extractColumnIndex<-function(tib, colName){
  if(!is.null(colName) && !is.null(tib)){
    #if(!("tbl_df" %in% class(tib))){ stop("extractColumnIndex: tib is not a tibble")}
    match(colName, names(tib), nomatch=NULL)
  } else {
    NULL
  }
}

extractSafeRowColIndex<-function(tib, rowIndex, colName){
  if(missing(tib) || missing(rowIndex) || missing(colName)){ stop("extractColumnIndex: missing param")}
  if(is.null(tib)){ return(NULL)}
  #if(!('tibble' %in% class(tib))){ stop("extractColumnIndex: tib is not a tibble")}
  if(is.null(colName)){return(NULL)}
  if(is.null(rowIndex)){ return(NULL) }
  if(rowIndex<1 || rowIndex>nrow(tib)){return(NULL)}
  colIndex<-match(colName, names(tib), nomatch=NULL)
  if(is.null(colIndex)){ return(NULL)}
  list(rowIndex=rowIndex, colIndex=colIndex)
}



#tib<- ptR$x
# 
# 
# #returns TRUE if all numeric
# isNumericString<-function(x){
#   all(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x))
# }


# splits tib at point given by rowIndex, ptIndex
# tagTib<-function(tib, ptCol,  rowIndex=nrow(tib), ptIndx){
#   tmp<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),]) # clones row
#   pts<-tmp[[rowIndex, ptCol]] # extract pts in that row and
#   ini<-1:(ptIndx-1)
#   tmp[[rowIndex,ptCol]]<-pts[,ini] # keep ini
#   tmp[[(rowIndex+1),ptCol]]<-pts[,-ini] # remove ini
#   tmp
# }

# y=util to discover which colunns might be points
tagTib<-function(tib, ptColIndex,  rowIndex=nrow(tib), matCol){
  cat('-----------------inside tagTib-------------')
  cat('ptColIndex=', ptColIndex,"\n")
  cat('rowIndex=', rowIndex,"\n")
  cat('matCol=', matCol,"\n")
  print(tib)
  tmp<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
  pts<-tmp[[rowIndex, ptColIndex]]
  # tmp[[rowIndex,ptColIndex]]<-pts[,1:(matCol-1)]
  # tmp[[(rowIndex+1),ptColIndex]]<-pts[,-(1:(matCol-1))]
  tmp[[rowIndex,ptColIndex]]<-matrix(pts[,1:(matCol)],2)
  tmp[[(rowIndex+1),ptColIndex]]<-matrix(pts[,-(1:(matCol))],2)
  tmp
}



# must rethink!

# 1)if row is empty, should we 
#   A) remove row?
#   B) and if go to previous row?
#   C) and then set pointIndex to the no of cols of the matrix
# 2) if after removing, a row is empty ptIndx=0, ow ptIndx=oldptIndx?


# Should we have the same for matrix??? (but ignore row unless tagged?)
#  add point
#  delete point
#  replace point
