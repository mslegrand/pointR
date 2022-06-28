# # sole caller: extractSafeRowColIndex (below)
# extractColumnIndex<-function(tib, colName){
#   if(!is.null(colName) && !is.null(tib)){
#     #if(!("tbl_df" %in% class(tib))){ stop("extractColumnIndex: tib is not a tibble")}
#     match(colName, names(tib), nomatch=NULL)
#   } else {
#     NULL
#   }
# }
# 
# # !!! not called
# extractSafeRowColIndex<-function(tib, rowIndex, colName){
#   if(missing(tib) || missing(rowIndex) || missing(colName)){ stop("extractColumnIndex: missing param")}
#   if(is.null(tib)){ return(NULL)}
#   #if(!('tibble' %in% class(tib))){ stop("extractColumnIndex: tib is not a tibble")}
#   if(is.null(colName)){return(NULL)}
#   if(is.null(rowIndex)){ return(NULL) }
#   if(rowIndex<1 || rowIndex>nrow(tib)){return(NULL)}
#   colIndex<-match(colName, names(tib), nomatch=NULL)
#   if(is.null(colIndex)){ return(NULL)}
#   list(rowIndex=rowIndex, colIndex=colIndex)
# }
# 


# sole caller: observeEvent( returnValue4ModuleRtFtr$tagPt() in serverFooterRight.R
# y=util to discover which colunns might be points
tagTib<-function(tib, ptColIndex,  rowIndex=nrow(tib), matCol){
  # cat('-----------------inside tagTib-------------')
  tmp<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
  pts<-tmp[[ ptColIndex]][[rowIndex]]
  if(matCol>0){
    tmp[[ ptColIndex]][[rowIndex]]<-matrix(pts[,1:(matCol)],2)
    tmp[[ ptColIndex]][[rowIndex+1]]<-matrix(pts[,-(1:(matCol))],2)
  }
  
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
