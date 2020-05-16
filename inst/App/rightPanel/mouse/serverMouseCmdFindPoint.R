mouseCmdFindPoint<-function(mssg){
  if(length(mssg$vec)>0){
    pt<- as.numeric(unlist(mssg$vec))
  }
  
  browser()
  fromColumnName<-getTibColumnName()
  fromCol<-getTib()[[fromColumnName]]
  fromColType<-extractColType( fromCol)
  
  # to get assetName, ptColName, rowIndex of matrix closest to pt
  toVal<-Inf
  toName<-NULL
  toColName<- NULL
  tibs<-getPtDefs()$tib
  toRow<-0
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
    # if asset=toName contains a column with the same name as the current col
    # and it's column type is the same as the current col, then toColName<-fromColName
    if(fromColumnName %in% names(tibs[[toName]]) ){
      toCol<-tibs[[toName]][[fromColumnName]]
      toColType<-extractColType(toCol)
      if(identical(toColType, fromColType)){
        toColName<-fromColumnName
      }
    }
    
    if( mssg$shiftKey==TRUE){ #add row to rowGroupsDB
      if( getAssetName()==toName ){
        if( getTibRow()==toRow){
          updateRowPicker(session, "myTibRowCntrl", toggleGroup = toRow)
        } else {
          updateRowPicker(session, "myTibRowCntrl", addToGroup = toRow, selectRow = toRow )
        }
      } else { # asset name about to change: set rowGroupsDB() 
        rowGroupsDB.addRow( getTibTabId(), toName, toColName, toRow )
      } 
    }  
    
    updateSelected( name=toName, rowIndex=toRow, columnName=toColName )
  }
} 
