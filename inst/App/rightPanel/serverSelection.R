

selectedTibble <- reactiveValues(
  name="x", #NULL,       # name of current point array
  row=1,
  column=1, # !!! KLUDGE for now. default to last col?
  matCol=0,
  ptColName='x',
  index=0          #  
)




updateSelected<-function( name, row, column, matCol, point.index, ptColName ){
  if(!missing(name)){
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
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
  if(!missing(column)){
    selectedTibble$column=column
  }
}

getCode<-reactive({request$code})
getTibName<-reactive({selectedTibble$name}) #allw to be null only if tib is null
getTibNameChoices<-reactive({
  names(getPtDefs()$tib)
}) #allow to be null only if tib is null


getTibColumn<-reactive({selectedTibble$column})

getTibEntry<-reactive({
  name<-getTibName()
  row<-getTibRow()
  col<-getTibColumn()
  getPtDefs()$tib[[getTibName()]][[row,col]]
})

getTibEntryChoices<-reactive({
  name<-getTibName()
  col<-getTibColumn()
  getPtDefs()$tib[[getTibName()]][,col]
})

getTib<-reactive({ getPtDefs()$tib[[ getTibName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})
getTibPts<-reactive({ 
  if( !is.null(selectedTibble$ptColName)){
    getTib()[[ selectedTibble$ptColName ]]
  } else {
    NULL
  }
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

getTibColumnName<-reactive({
  names(getTib())[ selectedTibble$column ]
  
  })
getTibColumnNameChoices<-reactive({ 
  tib<-getTib()
  if(!is.null(tib)){
    names(tib)
  } else {
    ""
  }
})

getTibMatCol<-reactive({ selectedTibble$matCol })
# getTibPos<-reactive({
#   list(row=selectedTibble$row, matCol=selectedTibble$matCol)
# })
getTibMatColChoices<-reactive({ 
  row<-getTibRow()
  pts<-getTibPts()
  rtv<-0
  if(row %in% getTibRowChoices() ){ #row check
    mc<-ncol(pts[[row]])
    if(length(mc)>0 && mc>0){ 
      rtv<-1:mc
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
