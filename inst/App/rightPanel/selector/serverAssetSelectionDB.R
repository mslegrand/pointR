


serverAssetDB<-reactiveValues( tib=initialServerAssetDB() )


storeAssetState<-function(){ 
  #log.fin(storeAssetState)
  selectionList<-reactiveValuesToList(selectedAsset, all.names=TRUE)
  if(is.null(selectionList$tabId) || identical(selectedAsset$tabId,'bogus')){
    #cat('nothing to store\n')
    return()
  }
  selectionList[sapply(selectionList,is.null)]<-NA
  tmp1<-filter(serverAssetDB$tib, tabId!=selectionList$tabId)
  serverAssetDB$tib<-bind_rows(tmp1, as_tibble(selectionList))
  #log.fout(storeAssetState)
}

#~ @param nextTabId
#~ @section sets selectedAsset according to  entry corresponding to tabId==nextTabId
# sole caller: processMssgFromAceMssgPageIn

# the call to getRightPanelChoices is problematic:
#  if an error state occurs, choice is errorPanelTag, ouch
#  otherwise depends on getSourceType and getptDefs
#  sourceType is set in commit (for svgR, R, ...), thus
#  sourceType may be invalid during the call to restoreWorkSpace.
#  The only justification would be if nextTabId is in serverAssetDB$tib
restoreAssetState<-function(nextTabId){
  if(length(nextTabId)==1 && !is.na(nextTabId)){
    # log.fin(restoreAssetState)
      
       if(nrow(serverAssetDB$tib)>0){
         row.tib<-filter(serverAssetDB$tib, tabId==nextTabId)
       } else {
         row.tib<-serverAssetDB$tib
       }
      if(length(row.tib)==0){
        cat(" length(row.tib)==0\n"); 
        browser() #should never happen
      }

      if(nrow(row.tib)==0 || length(row.tib) <length(names(selectedAsset))){

        choices<-getRightPanelChoices() 
        row.tib<-newAssetSelection(tabId=nextTabId, choices=choices, tibs=getPtDefs()$tib)
      }
      if(!is.null(row.tib)){
        lapply(names(row.tib), function(n){
          v<-row.tib[[n]][1]
          if(!is.null(v) && is.na(v)){v<-NULL} 
          selectedAsset[[n]]<-v
        } )
      }
    # log.fout(restoreAssetState)
  }
}

# called only by restoreAssetState
newAssetSelection<-function( tabId, choices, tibs){
  # log.fin(newAssetSelection)
  if( length(tabId)==0 || length(choices)==0){
    return( NULL)
  }
  #create a tibble
  name=choices[1]
  
  if( is.null(tibs)){
    rowIndex=1
    columnName='x' #bogus
    matCol=0
    ptColName='x'  #bogus
  } else {
    tib<-tibs[[name]]
    rowIndex=nrow( tib )
    columnName<-tail(names(tib),1)
    ptIndxs<-extractPointColumnIndices(tib)
    if(length(ptIndxs)>0){
      ptColIndex<-ptIndxs[1]
      entry<-tib[[rowIndex,ptColIndex]]
      ptColName<- names(tib)[ptColIndex]
      columnName<-ptColName
      matCol<-ncol(entry)
      selIndex=1
    } else {
      ptColName<-NULL
      matCol<-0
    }
    
    if(name==transformTag){
      transformType='Translate'
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
    transformType='Translate',
    ptScriptSel=preprocChoices$points[1]
  )
  # log.fout(newAssetSelection)
  selection
}