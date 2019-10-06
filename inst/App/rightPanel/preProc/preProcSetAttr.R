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


# appendAttrValues<-function(tib, =TRUE, ...){
#   vals<-list(...)
#   trow<-top_n(tib,1)
#   trow[,names(vals)]<-vals
#   if(missing==T){
#     aNames<-names(vals)
#     missingNames<-setdiff(names(tib),anames)
#     for(mName in missingNames){
#       colVal<-tib[[mName]]
#       if(is.list(colVal)){
#         clsses<-sapply(colVal, function(x) class(x))
#         if(all(identical(clsses,'matrix'))){
#           allNum<-sapply(colVal, function(x)is.numeric(x))
#           if(all(unlist(allNUM))){ # all numeric
#             allRowNum<-sapply(colVal, function(x){ identical(nrow(x),as.integer(2))})
#             if(all(allRowNum)){
#                # add mName=matrix(0,2,0) to list
#             }
#           }
#          
#         }
#       } 
#       else {
#         
#       }
#     }
#     missingPtNames<-sapply(missingNames, function(bn){if(isPoints(tib[[bn]])) matrix(0,2,0) else NULL})
#   }   
#   tib<-bind_rows(tib, trow)
#   tib
# }
#cloneSetNAppendRow
#rowCSAppend
#rowSAppend

# NAME CONFLICT *getTibRow*, CHANE TO ... EXTRACTLASTROW( context, tibName=context$name)
# getTibRow<-function(context){
#   assetName<-context$name
#   tibs<-context$tibs
#   if( !is.null(tibs) &&
#       is_scalar_character(assetName) && 
#       assetName %in% names(tibs) 
#   ){
#     tib<-tibs[[ assetName ]]
#     rowIndex<-   max(nrow(tib),context$row)
#     return( tib[rowIndex,])
#   }
# }
# 
# CHANGE TO EITHER REPLACELASTROW, APPENDROWTOEND
# setTibRow<-function(context, replacmentRow){
#   assetName<-context$name
#   tibs<-context$tibs
#   if( !is.null(tibs) &&
#       is_scalar_character(assetName) && 
#       assetName %in% names(tibs) 
#   ){
#     tib<-tibs[[ assetName ]]
#     rowIndex<-   max(nrow(tib),context$row)
#     if(rowIndex>0 && 
#        All( names(replacmentRow) %in% names(tib))
#     ){
#       tib[rowIndex,names(replacmentRow)]=replacementRow
#       context$tibs[[asetName]]<-tib
#     }
#   }
# }
