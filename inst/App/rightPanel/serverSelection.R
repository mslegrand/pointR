selectedPoint <- reactiveValues(
  name="x", #NULL,       # name of current point array
  point.index=0          #  selected pt.indx (column) in current point array
)


selectedTibble <- reactiveValues(
  name="x", #NULL,       # name of current point array
  row=1,
  column=1, # default to last col?
  matCol=0,
  ptColName='pts',
  index=0          #  
)




updateSelected<-function( name, row, column, matCol, point.index, ptColName ){
  if(!missing(name)){
    selectedPoint$name=name
    selectedTibble$name=name
  }
  if(!missing(point.index)){
    cat("updateSelected: setting point.index=",point.index,"\n")
    selectedPoint$point.index=as.numeric(point.index)
  }
  if(!missing(ptColName)){
    selectedTibble$ptColName=ptColName
  }
  if(!missing(row)){ # !!! may want to provide a check here
    selectedTibble$row=row
    cat("updateSelected: setting row=",row,"\n")
    # point.index<-selectedPoint$point.index
    # if( row>0 ){
    #   endPos<-getTibPtsColEndIndex()
    #   begPos<-c(0,endPos)+1
    #   selectedPoint$point.index<-point.index<-min(max(point.index,begPos[row]),endPos[row])
    # } 
  }
  if(!missing(matCol)){
    cat("updateSelected: setting matCol=",matCol,"\n")
    selectedTibble$matCol=matCol
  }
  if(!missing(column)){
    selectedTibble$column=column
  }
}

# selectedPoint <- reactiveValues(
#   tibble.name="x", #NULL,       # name of current point array
#   point.index=0,          #  selected pt.indx (column) in current point array
#   point.col.name='pts',
#   row.num=1
# )





#--- yes unless tagged with freq or no points to tag 
# isTaggable<-reactive({ 
#   name<-getPtName()
#   !is.null(name) && getPtIndex()>0 &&  is.null(reactiveTag$freq[[name]])
# })

getCode<-reactive({request$code})
getPtName<-reactive({selectedPoint$name})
getTibName<-reactive({selectedTibble$name}) #allw to be null only if tib is null

getPtIndexChoices<-reactive({ 
  isolate(print(getTibPts() ))
  t<-length(unlist(getTibPts()))/2
  b<-min(1,t)
  b:t
})

getPtIndex<-reactive({
  if(length(selectedPoint$point.index)>0){
    as.numeric(selectedPoint$point.index)
  } else {
      max(getPtIndexChoices())
  }
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

getTibPtsColEndIndex<-reactive({
  cs<-getTibPtsNCol()
  if(length(cs)>0){
    cs<-cumsum(cs)
  }
  cs
})


#!!!TODO THIS WILL FAIL IF WE HAVE MUTLIPLE EMPTY MATRICES, FIX ALGORITHM !!!
absPtIndx2TibPtPos<-function(indx){
  # cat("Enter: absPtIndx2TibPtPos\n")
  # cat("(point.index) indx=",indx,"\n")
  # cat("length(indx)=",length(indx),"\n")
  rtv<-list(row=1,matCol=0)
  if(length(indx)>0 && indx>0){
    #tib<-ptDefs()$tib
    mlen<-sapply(getTibPts(),ncol)
    if(sum(mlen)<indx){
      #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
      return(NULL)
    }
    #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
    if(length(mlen)>0){
      endpts<-cumsum(mlen)
      #cat("endpts=c(",paste(endpts,collapse=","),")\n")
      begpts<-c(1, (endpts+1)[-length(endpts)])
      #cat("begpts=c(",paste(begpts,collapse=","),")\n")
      r<-sum(indx>=begpts)
      #cat("r=",r,"\n")
      if(r>0){
        matCol<-indx-(begpts[r]-1)
        rtv<-list(row=r,matCol=matCol)
      }
    }
  }
  #cat("rtv=list( row=",rtv$row,", matCol=",rtv$matCol,")\n")
  #cat("Exit: absPtIndx2TibPtPos\n\n")
  rtv
}

# getTibPtPos<-reactive({ # alternatively use observers and set row, index
#   #pts<-getTibPts()
#   cs<-getTibPtsColEndIndex()
#   
#   indx<-getPtIndex()
#   sum(indx<=cs)->r
#   cs<-c(0,cs)
#   rindx<-indx-cs[r]
#   list(row=r,matColPos=rindx)
# })

# observe({ #An alternative to getTibPtPos, updates tibble index, row whenever point.index changes
#   cs<-getTibPtsColEndIndex()
#   indx<-getPtIndex()
#   isolate({
#     #browser()
#     if(length(indx)==0 || indx==0){
#       selectedTibble$index<-0
#       selectedTibble$row<-0
#     } else {
#       sum(indx<=cs)->r
#       if(r>0){
#         cs<-c(0,cs)
#         selectedTibble$index<-indx-cs[r]
#         selectedTibble$row<-r
#       }
#     }
# 
#   })
# })

# tibptPos can change if 
#  1. index changes
#  2. name changes
#  3. tagging occurs
#     note: 2 or 3 implies have changed 

getTibRow<-reactive({selectedTibble$row})
getTibRowChoices<-reactive({ 
  tib<-getTib()
  if(!is.null(tib) && is.finite(nrow(tib)) && nrow(tib)>0 ){
    1:nrow(tib) 
  } else {
    1
  }
})

getTibMatCol<-reactive({ selectedTibble$matCol })

getTibMatColChoices<-reactive({ 
  tib<-getTib()
  if(!is.null(tib) && is.finite(nrow(tib)) 
     && nrow(tib)>0 
     && getTibRow()<nrow(tib)
     && !is.null(getTibPtColPos() )
  ){
    m<-tib[[getTibRow() ,getTibPtColPos() ]] 
    1:ncol(m)
  } else {
    0:0
  }
})



#getTibIndex<-reactive({selectedTibble$index})



#inverse function : not used
tibPtPos2AbsPtIndx<-reactive({
  cs<-getTibPtsNCol()
  if(length(nCols)>0){
    cs<-cumsum(cs)
  }
  function(row, matCol){
    if( row>0 && matCol>0 && length(cs)>0 ){
      sum(cs[1:row])+matCol
    } else {
      0
    }
  }
})


#-----------------------



#gets the tagged names
getTagNameChoices<-reactive({
  names(getPtDefs()$tib)
  #intersect(names(getPtDefs()$pts), names(getPtDefs()$df))
})

# getSelectInfo<-reactive({ #used by pointsBar only??
#   name<-getPtName()
#   indx<-getPtIndex()
#   pts<-getPtDefs()$pts
#   ex.getSelectInfo(pts, name, indx)
# })

# getPts<-reactive({
#   ptdef<-getPtDefs()
#   ptdef[[getPtName()]]
# })

#gets a tagged name (=ptName unless ptName is not tagged)
getTagName<-reactive({
  #exGetTagName( getTagNameChoices(), getPtName() )
  getTibName()
})

#uses getTibPts
getTagIndexChoices<-reactive({  
  #getPtDefs()$df[[getTagName()]]$tag
  nCols<-sapply(getTibPts(),ncol)
  if(length(nCols)>0){
    endPos<-cumsum(nCols)
    begPos<-c(1,1+endPos[-length(endPos)])
  } else {
    begPos<-0
  }
  begPos
})

#uses pointIndex
getTagIndex<-reactive({ 
  #choices<-getTagIndexChoices()
  indx<-getPtIndex()
  if(length(indx)>0){
    ch<-getTagIndexChoices()
    max(ch[indx>=ch])
  } else
    0
})




getTagDF<-reactive({
  ptName<-getPtName()
  if(is.null(  getPtName() )){
    ptTags<-NULL
  }
  tagRList<-getPtDefs()$df 
  if(!is.null(tagRList)){
    ptTags<-tagRList[[ptName]]
  } else {
    ptTags<-NULL
  }
  ptTags
})
