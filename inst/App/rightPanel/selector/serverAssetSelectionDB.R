
# serverAssetDB<-reactiveValues(
#   tib=tibble(
#     tabId="NULL",
#     name="NULL",
#     rowIndex=1,         # row in tibble
#     columnName="NULL",  # currently used only by tibbleEditor and could be placed there.
#     matCol=0,           # colIndex of the current matrix.
#     ptColName="NULL",   # !!! KLUDGE for now. should this default to last col?
#     selIndex=1,         # only used when current col is points,
#     transformType='Translate',
#     ptScriptSel=preprocChoices[1]
#   )[0,]
# )

serverAssetDB<-reactiveValues( tib=initialServerAssetDB() )


storeAssetState<-function(){ 
  selectionList<-isolate(reactiveValuesToList(selectedAsset, all.names=TRUE))
  if(is.null(selectionList$tabId) || identical(selectedAsset$tabId,'bogus')){
    return()
  }
  selectionList[sapply(selectionList,is.null)]<-NA
  tmp1<-filter(serverAssetDB$tib, tabId!=selectionList$tabId)
  serverAssetDB$tib<-bind_rows(tmp1, as.tibble(selectionList))
}

#~ @param nextTabId
#~ @section sets selectedAsset according to  entry corresponding to tabId==nextTabId

# the call to getRightPanelChoices is problematic:
#  if an error state occurs, choice is errorPanelTag, ouch
#  otherwise depends on getSourceType and getptDefs
#  sourceType is set in commit (for svgR, R, ...), thus
#  sourceType may be invalid during the call to restoreWorkSpace.
#  The only justification would be if nextTabId is in serverAssetDB$tib
restoreAssetState<-function(nextTabId){
  if(length(nextTabId)==1 && !is.na(nextTabId)){
       # browser()
       if(nrow(serverAssetDB$tib)>0){
         row.tib<-filter(serverAssetDB$tib, tabId==nextTabId)
       } else {
         row.tib<-serverAssetDB$tib
       }
      if(nrow(row.tib)==0){
       #  browser()
        choices<-getRightPanelChoices() # this is suspect
        row.tib<-newAssetSelection(tabId=nextTabId, choices=choices, tibs=getPtDefs()$tib)
      }
      if(!is.null(row.tib)){
        lapply(names(row.tib), function(n){
          v<-row.tib[[n]][1]
          if(is.na(v)){v<-NULL} 
          selectedAsset[[n]]<-v
        } )
      }
  }
}

# called only by restoreAssetState
newAssetSelection<-function( tabId, choices, tibs){
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
    transformType='Translate',
    ptScriptSel=preprocChoices[1]
  )
}