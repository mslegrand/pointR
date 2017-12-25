

selectedTibble <- reactiveValues(
  name="x",        # name of current point array
  row=1,
  columnName='x', # currently used only by tibbleEditor and could be placed there.
  matCol=0,
  ptColName='x'      # !!! KLUDGE for now. should this default to last col?
)


resetSelectedTibbleName<-function(name, tibs){
  if(is.null(name) || nchar(name)==0){ return(NULL)}
  if(is.null(tibs) || is.null(names(tibs)) || length(names(tibs))==0){ return(NULL)}
  if(!(name %in% names(tibs))){ name=tibs[[1]]}
  
  
  #set name
  selectedTibble$name=name
  tib<-tibs[[name]]
 
  # !!!KLUDGE for now, ASSUME TIB NAME AND POINTS NAME ARE SAME!!!
  ptColName=name
  # !!! TODO:  find and set point column
  #set ptColName
  selectedTibble$ptColName=ptColName 
  #set columnName to be ptColName
  colName=ptColName
  selectedTibble$columnName=colName
  
  # set row
  rowIndex=nrow( tib )
  selectedTibble$row=rowIndex
  
  #next set matCol 
  matColIndex<-0 # zero by default, reset if we can find a matrix entry
  indices<-extractSafeRowColIndex(tib, rowIndex, ptColName)
  if(!is.null(indices)){
    entry<-tib[[indices$rowIndex, indices$colIndex]]
    if( is.matrix(entry) && dim(entry)[1]==2 ){ 
      matColIndex<-ncol(entry)
    } 
  }
  selectedTibble$matCol<-matColIndex
  pts<-tibs[[selectedTibble$name]][[selectedTibble$ptColName]] 
}

updateSelected<-function( name, row, columnName, matCol,  ptColName ){
  if(!missing(name)){
    cat("updateSelected: setting name=",name,"\n")
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
    cat("updateSelected: setting ptColName=",name,"\n")
    selectedTibble$ptColName=ptColName
  }
  if(!missing(row)){ # !!! may want to provide a check here
    selectedTibble$row=row
    cat("updateSelected: setting row=",row,"\n")
  }
  if(!missing(matCol)){
    cat("updateSelected: setting matCol=",matCol,"\n")
    if(matCol=='end'){
      mc<-ncol(getTibPts()[[selectedTibble$row]])
      selectedTibble$matCol = ifelse(is.integer(mc), mc, 0)
    } else {
      selectedTibble$matCol=matCol
    }
  }
  
  if(!missing(columnName)){
    cat("updateSelected: setting columnName=",columnName,"\n")
    selectedTibble$columnName=columnName
  }
}

getCode<-reactive({request$code})
getTibName<-reactive({selectedTibble$name}) #allow to be null only if tib is null
getTibNameChoices<-reactive({
  ptDefs<-getPtDefs()
  if(is.null(ptDefs)){
    cat('\n===============getTibNameChoices:: ptDefs is NULL tib')
  } else {
    cat('\n==============getTibNameChoices:: ptDefs is NOT NULL tib\n')
    cat('\ngetTibNameChoices:: length(choices)=', length(names(getPtDefs()$tib)),"\n")
    cat('\ngetTibNameChoices:: choices=', paste(names(getPtDefs()$tib),collapse=", "),"\n")
  }
  # rtv<-names(getPtDefs()$tib)
  
  #browser()
  names(getPtDefs()$tib)
}) #allow to be null only if tib is null

getTibColumnName<-reactive({
  if( !is.null(selectedTibble$columnName)){
    cat("getTibColumnName()=",selectedTibble$columnName,"\n")
  }
  selectedTibble$columnName
})
getTibColumnNameChoices<-reactive({ 
  tib<-getTib()
  choices<-tib %AND% names(tib)
  choices
})

getTibColumn<-reactive({
  colName<-getTibColumnName()
  if(!is.null(colName) && nchar(colName)>0){
    columnNameChoices=getTibColumnNameChoices()
    ptPos<-getTibPtColPos()
    column<-match(colName, columnNameChoices, nomatch=ptPos)
  } else {
    column<-NULL
  }
  column
})

getTibEntry<-reactive({
  name<-getTibName()
  rowNum<-getTibRow()
  columnName<-getTibColumnName()
  # if name, row or colName is NULL or NA return NULL 
  # if colname not in tib return NULL
  # if row not in
  cat("\n-----Entering-----getTibEntry::----------------\n")
  tib<-name %AND% getPtDefs()$tib[[name]]
  cat("getTibEntry:: class(name)=",class(name),"\n")
  cat("getTibEntry:: class(tib)=",class(tib),"\n")
  cat("getTibEntry:: class(columnName)=",class(columnName),"\n")
  columnValues<- columnName %AND% tib[[columnName]]
  cat("getTibEntry:: class(columnValues)=",class(columnValues),"\n")
  trows<-columnValues %AND% length(columnValues)
  cat("getTibEntry:: class(trows)=",class(trows),"\n")
  cat("getTibEntry:: class(row)=",class(rowNum),"\n")
  # entry<-trows %AND% row %AND% 
  #   (
  #     if(1<=row && row<=trows){
  #       cat("row=" ,row,"\n")
  #       cat("columnName=" ,columnName,"\n")
  #       cat("class(tib[[columnName]][[row]])=" ,class(tib[[columnName]][[row]]),"\n")
  #       tib[[columnName]])[[row]]
  #     }else{
  #        NULL
  #     }
  #   )
  entryOk<-trows %AND% rowNum %AND% (if(1<=rowNum && rowNum<=trows){ TRUE } else { NULL})
  if(!is.null(entryOk)){
     entry<- as.list(tib[[columnName]])[[rowNum]]
     print(entry)
  } else {
    entry<-NULL
  }
   
  cat("\n-----Exiting-----getTibEntry::----------------\n")
  entry
})

getTibEntryChoices<-reactive({
  cat("\n-----Entering-----getTibEntryChoices::----------------\n")
  cat("getTibEntryChoices::\n")
  name<-getTibName()
  columnName<-getTibColumnName()
  tib<-name %AND% getPtDefs()$tib[[name]]
  columnValues<- columnName %AND% tib[[columnName]]   
  #columnValues<-columnValues %AND% as.list(columnValues)
  
  columnValues <-columnValues %AND% as.list(columnValues)
  cat("\n-----Exiting-----getTibEntryChoices::----------------\n")
  columnValues
})

getTib<-reactive({ getTibName() %AND% getPtDefs()$tib[[ getTibName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})

getTibPts<-reactive({ 
  ptCol<-selectedTibble$ptColName
  tib<-getTib()
  pts <- tib %AND% ptCol %AND% tib[[ptCol]]
  pts
})


getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )

# getTibPtsColEndIndex<-reactive({
#   cs<-getTibPtsNCol()
#   if(length(cs)>0){
#     cs<-cumsum(cs)
#   }
#   cs
# })
# 

# #!!!TODO THIS WILL FAIL IF WE HAVE MUTLIPLE EMPTY MATRICES, FIX ALGORITHM !!!
# absPtIndx2TibPtPos<-function(indx){
#   # cat("Enter: absPtIndx2TibPtPos\n")
#   # cat("(point.index) indx=",indx,"\n")
#   # cat("length(indx)=",length(indx),"\n")
#   rtv<-list(row=1,matCol=0)
#   if(length(indx)>0 && indx>0){
#     #tib<-ptDefs()$tib
#     mlen<-sapply(getTibPts(),ncol)
#     if(sum(mlen)<indx){
#       #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
#       return(NULL)
#     }
#     #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
#     if(length(mlen)>0){
#       endpts<-cumsum(mlen)
#       #cat("endpts=c(",paste(endpts,collapse=","),")\n")
#       begpts<-c(1, (endpts+1)[-length(endpts)])
#       #cat("begpts=c(",paste(begpts,collapse=","),")\n")
#       r<-sum(indx>=begpts)
#       #cat("r=",r,"\n")
#       if(r>0){
#         matCol<-indx-(begpts[r]-1)
#         rtv<-list(row=r,matCol=matCol)
#       }
#     }
#   }
#   rtv
# }
# 

getTibRow<-reactive({selectedTibble$row})
getTibRowChoices<-reactive({ 
  tib<-getTib()
  if(!is.null(tib) && is.finite(nrow(tib)) && nrow(tib)>0 ){
    1:nrow(tib) 
  } else {
    1
  }
})



getTibMatCol<-reactive({ 
  cat( "selectedTibble$matCol=", selectedTibble$matCol ,"\n" )
  selectedTibble$matCol 
})

# getTibPos<-reactive({
#   list(row=selectedTibble$row, matCol=selectedTibble$matCol)
# })

getTibMatColChoices<-reactive({ 
  rowNum<-getTibRow()
  pts<-getTibPts()
  if(is.null(pts) || is.null(rowNum) || rowNum<1 || rowNum>length(pts)){
    rtv<-NULL
  } else {
    mc<-ncol(pts[[rowNum]])
    if(mc>0){
      rtv<-1:mc
    } else {
      rtv<-0
    }
  }
  rtv
})

#inverse function : !!!WARNING assumes no empty rows (gaps)
# tibPtPos2AbsPtIndx<-reactive({
#   pts<-getTibPts()
#   function(row, matCol){
#     cs<-sapply(pts,ncol)
#     if(length(cs)>0 && row>0  ){
#       cs<-c(0,cumsum(cs))
#       cs[row] +matCol
#     } else {
#       0
#     }
#   }
# })

# ptPos2AbsPtIndx<-function(pts, row, matCol  ){
#   if(!is(pts,'list') || !is(row, 'integer') || !is(matCol, 'integer')){
#     cat("class(pts)=", class(pts),"\n")
#     cat("class(row)=", class(row),"\n")
#     cat("class(matCol)=", class(matCol),"\n")
#     #stop('cannot compute ptIndex')
#   }
#   if( length(pts)<row ){
#     stop('row number is out of bounds of points')
#   }
#   cs<-sapply(pts, ncol)
#   if(length(cs)>0 && row>0  ){
#     cs<-c(0,cumsum(cs))
#     cs[row] +matCol
#   } else {
#     0
#   }
# }
