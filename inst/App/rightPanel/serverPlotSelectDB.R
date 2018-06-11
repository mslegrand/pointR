
plot<-reactiveValues(
selections.tib=tibble(
  tabId="NULL",
  name="NULL",
  rowIndex=1,         # row in tibble
  columnName="NULL",  # currently used only by tibbleEditor and could be placed there.
  matCol=0,           # colIndex of the current matrix.
  ptColName="NULL",   # !!! KLUDGE for now. should this default to last col?
  selIndex=1,         # only used when current col is points, 
  transformType='Translate'    
)[0,]
)


# store<-function(){
#   
# }
# 
# restore<-function(tabId){
#   filter(plotSelect.tib, tabId=tabId)
# }
# 


newPlotSel<-function( tabId, choices, tibs){
  if( length(tabId)==0 || length(choices)==0){
    return( NULL)
  }
  #create a tibble
  name=choices[1]
  if( is.null(tibs)){
    rowIndex=1
    columnName='x'
    matCol=0
    ptColName='x'
  } else {
    tib<-tibs[[name]]
    rowIndex=nrow( tib )
    ptIndxs<-sapply(  seq_along(names(tib)),function(j){
        is.matrix(tib[[rowIndex,j]]) && dim(tib[[rowIndex,j]])[1]==2
      } 
    )
    ptIndxs<-which(ptIndxs==T)
    if(length(ptIndxs)>0){
      ptColIndex<-ptIndxs[1]
      entry<-tib[[rowIndex,ptColIndex]]
      ptColName<- names(tib)[ptColIndex]
      matCol<-ncol(entry)
      selIndex=1
    } else {
      ptColName<-NULL
      matCol<-0
    }
    columnName<-ptColName
    if(name==transformTag){
      transformType='translate'
    }
  }
  selection=list(
    tabId=tabId,
    name=name,
    rowIndex=rowIndex,
    columnName=columnName,
    matCol=matCol,
    ptColName=ptColName,
    selIndex=1,
    transformType='Translate'
  )
  
}