mouseCmdFindPoint<-function(mssg){
  if(length(mssg$vec)>0){
    pt<- as.numeric(unlist(mssg$vec))
  }
  tibs<-getPtDefs()$tib
  # to get assetName, ptColName, rowIndex of matrix closest to pt
  toVal<-Inf
  toName<-NULL
  toColName<- NULL
  toRow<-0
  browser()
  for(name in names(tibs)){
    tib<-tibs[[name]]
    for(colName in names(tib)){
      ctype<-extractColType(tib[[colName]])
      if(ctype=='point') {
        ptCol<-tib[[colName]]
        for(i in 1:nrow(tib)){
          m<-ptCol[[i]]
          if(length(m)>0){
            v<-min( apply( (m-pt)^2, 2, sum))
            if(v<toVal){
              toVal<-v
              toName<-name
              toColName<- colName
              toRow<-i
            }        
          }
        }
      }
    }
  }
  # if found change current selection to 
  if(toVal<Inf){
    updateSelected( name=toName, rowIndex=toRow, columnName=toColName )
  }
} 
