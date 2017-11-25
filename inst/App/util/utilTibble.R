library(tidyverse)

ptR<-list(
  
  x=tibble(
    pts=list(matrix(1:6,2), matrix(11:14,2)),
    fill=c('red','white')
  )

)

# getPtRTibble<-function(ptDef, tibble.name){
#   ptDef[[tibble.name]]
# }
# 
# getPtRColumn<-function(ptDef, tibble.name, col.name){
#   ptDef[[tibble.name]][[col.name]]
# }
# 
# #setPtrColumn
# setPtRColumn<-function(ptDef, tibble.name, col.name, colVal){
#   ptDef[[tibble.name]][[col.name]]<-colVal
# }
# 
# getPtREntry<-function(ptDef, tibble.name, col.name, row.index ){
#   ptDef[[tibble.name]][[col.name]][[row.index]]
# }
# 
# setPtREntry<-function(ptDef, tibble.name, col.name, row.index, value ){
#   ptDef[[tibble.name]][[col.name]][[row.index]]<-value
# }
# 
# getPtRRow<-function(ptDef, tibble.name, row.index){
#   ptDef[[tibble.name]][row.index,]
# }
# 
# # appendRow
# # clone last row and add 
# appendRow<-function(ptDef, tibble.name ){
#   indx<-nrow(ptDef[[tibble.name]])
#   if( indx>0 ){
#     # get last row
#     temp<-getPtRRow(ptDef,tibble.name, indx)
#     # empty any matrix (ie. points)
#     tempM<-sapply(ptR$y, function(x) class(x[[1]])=='matrix')
#     temp[1,tempM]<-c()
#     temp2<-rbind(ptDef[[tibble.name]], temp)
#     temp2
#   } else {
#     ptDef[[tibble.name]]
#   }
# }
# 
# 
# insertRow
# 
# removeRpw
# 
# swapRows
# 
# cloneRow
# 

tib<- ptR$x


#returns TRUE if all numeric
isNumericString<-function(x){
  all(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x))
}

#clones rowIndex and places new row at end

cloneTib<-function(tib, rowIndex=nrow(tib)){
  bind_rows(tib, tib[rowIndx,])
}


#clones rowIndex and places new row at immediately after rowIndex
# todo: inc rowIndex
cloneTibAtRow<-function(tib, rowIndex=nrow(tib)){
  bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
}

# splits tib at point given by rowIndex, ptIndex
tagTib<-function(tib, ptCol,  rowIndex=nrow(tib), ptIndx){
  tmp<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),]) # clones row
  pts<-tmp[[rowIndex, ptCol]] # extract pts in that row and
  ini<-1:(ptIndx-1)
  tmp[[rowIndx,ptCol]]<-pts[,ini] # keep ini
  tmp[[(rowIndx+1),ptCol]]<-pts[,-ini] # remove ini
  tmp
}

# y=util to discover which colunns might be points
tagTib<-function(tib, ptCol,  rowIndex=nrow(tib), ptIndx){
  tmp<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
  pts<-tmp[[rowIndex, ptCol]]
  tmp[[rowIndx,ptCol]]<-pts[,1:(ptIndx-1)]
  tmp[[(rowIndx+1),ptCol]]<-pts[,-(1:(ptIndx-1))]
  tmp
}

extendTib<-function(tib){
  rowIndx<-nrow(tib)
  if(rowIndx>0){
    tib<-bind_rows(tib, tib[rowIndx,])
    sapply(tib, function(x)class(x[[1]])=='matrix')->indx
    tib[rowIndx+1,indx]<-c()
  }
  tib
}

ptColTib<-function(tib){
  rowIndx<-nrow(tib)
  colIndx<-c()
  if(rowIndx>0){
    sapply(tib, function(x)class(x[[1]])=='matrix')->indx
    colIndx<-which(indx)
  }
  colIndx
}


# extract the given tib column
=
getColTib<-function(tib, indx){
  tib[[indx]]
}


# set the given tib column with the given value

setColTib<-function(tib, indx, value){
  tib[[indx]]<-value
}


# remove a row from the tib

removeRowTib<-function(tib, rowIndx){
  tib[-rowIndx,]
}


# swap rows of a tib
swapRowsTib<-function(tib, i, j){
  tib[c(i,j),]<-tib[c(j,i),]
  tib
}



# step selected point forward

stepPtForward<-function(tib, row, ptCol, ptNo){
  if(ptNo<dim(tib[[row,ptCol]])[2]){
      ptNo=ptNo+1
  } else {
    if(row<nrow(tib)){
          row=row+1
          ptNo=1
    }
  }
  return(
    list( row=row, ptNo= ptNo)
  )
}



# step selected point backward



stepPtBackward<-function(tib, row, ptCol, ptNo){
  if(ptNo>1){
    ptNo=ptNo-1
  } else {
    if(row>1){
      row=row+1
      ptNo=dim(tib[[row,ptCol]])[2]
    }
  }
  return(
    list( row=row, ptNo= ptNo)
  )
}

# Should we require that pt col is always a matrix?
# empty matrix is produced by 
#           matrix(list(), 2)
#
# !!! Todo, set ptIndx after adding, deleting, or changing.
# add should increment
# delete, replace should keep same


# add pt immediately after selected point

addPoint<-function(tib, row, ptCol, ptIndx, pt){
  ptMat<-tib[[row,ptCol]]
  if(length(ptMat)>0){
    ptMat<-cbind( ptMat[,1:(ptIndx)], pt, ptMat[,(ptIndx+1):dim(ptMat)[2]] )
  } else {
    ptMat<-matrix(pt,2)
  }
  tib[[row,ptCol]]<-ptMat
  tib
}

# must rethink!

# 1)if row is empty, should we 
#   A) remove row?
#   B) and if go to previous row?
#   C) and then set pointIndex to the no of cols of the matrix
# 2) if after removing, a row is empty ptIndx=0, ow ptIndx=oldptIndx?

# remove selected point

# if row is empty, should we 
# 1) remove row?
# 2) and if go to previous row?
# 3) if after removing row is empty ptIndx=0, ow ptIndx=oldptIndx?
# 4) if remove row, should reset pointIndex to length of row?

removePoint<-function(tib, row, ptCol, ptIndx){
  ptMat<-tib[[row,ptCol]]
  if(length(ptMat)>1){
    ptMat<- ptMat[,- ptIndx ]
  } else {
    ptMat<- matrix[list(),2]
  }
  tib[[row,ptCol]]<-ptMat
  tib
}


replacePoint<-function(tib, row, ptCol, ptIndx, pt){
  ptMat<-tib[[row,ptCol]]
  ptMat[,ptIndx ]<-pt
  tib[[row,ptCol]]<-ptMat
  tib
}




# Should we have the same for matrix??? (but ignore row unless tagged?)
#  add point
#  delete point
#  replace point
