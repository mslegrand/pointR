
# todo!!! rename selectedTibble to something better, like 
# currentPlotSelect or currentAsset or canvasState or 
# selectedAsset or assetState or assetManager or assetSelectionInfo
# or assetCoordinates or assetSelectionProperties
# or activeAssetPropteries activeAssetState *activeAssetInfo*
# add svg as a name when needed.

selectedAsset <- reactiveValues(
  tabId="bogus",
  name=NULL,        # name of current point array
  rowIndex=1,
  columnName=NULL, # currently used only by tibbleEditor and could be placed there.
  matCol=0, #
  ptColName=NULL,      # !!! KLUDGE for now. should this default to last col?
  selIndex=1, # only used is to determine if in matrix or point mode !! 
  transformType='Translate', # TODO!!! replace this with selIndex
  ptScriptSel=preprocChoices[1]
)



getSelIndex<-reactive({
  selectedAsset$selIndex
})

observeEvent(getTibNRow(),{
  if(!is.null(getPtDefs()$tib) && length(names(getPtDefs()$tib))>0 ){
  # if(identical(getMode(),'ptR') && length(names(getPtDefs()$tib))>0 ){
    sendPtRManagerMessage(  sender='tibNrow', rowCountChange=TRUE)
  }
}, label='serverAssetSelection:: getTibNRow()')

getAssetName<-reactive({selectedAsset$name}) #allow to be null only if tib is null  
getTibTabId<-reactive({ selectedAsset$tabId})
getTibColumnName<-reactive({ selectedAsset$columnName })
getTib<-reactive({ getAssetName() %AND% getPtDefs()$tib[[ getAssetName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedAsset$ptColName )})
getTibNRow<-reactive({
  if( getTibEditState()==TRUE ){
    nrow(getPtDefs()$tib[[getAssetName()]])
  } else {
    0
  }
})

atLeast2Rows<-reactive({
  getTibEditState()==TRUE && nrow(getPtDefs()$tib[[getAssetName()]])>1
})

getTibRow<-reactive({
  # cat('getTibRow()=',format( selectedAsset$rowIndex),"\n");
  selectedAsset$rowIndex})

getTibMatCol<-reactive({ 
  # cat('getTibMatCol::', format(selectedAsset$matCol), "\n")
  selectedAsset$matCol 
})
getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )

getTransformType<-reactive({ 
  if(is.null(selectedAsset$transformType)){
    'Translate'
  } else {
    selectedAsset$transformType
  }
})


getTibMatColMax<-reactive({
    rowNum<-getTibRow()
    pts<-getTibPts()
    if(is.null(pts) || is.null(rowNum) || rowNum<1 || rowNum>length(pts)){
      NULL
    } else {
      ncol(pts[[rowNum]])
    }
})

# this is called to reset the tib name when possible 
# used by 
#    1. serverEdtib to reset the name when the selection changes
#    2. serveAce to reset name when we have a file->New or file->Open
resetSelectedTibbleName<-function(tibs, name){
    cat('>---f-> resetSelectedTibbleName\n')
    if(hasError()){
      cat('<---f-< resetSelectedTibbleName\n')
      return(NULL) # never change selection when in error state
    }
      choices<-getRightPanelChoices()
      cat("resetSelectedTibbleName:: choices=", paste(choices, collapse=", "),"\n")
      if(is.null(name) || !(name %in% choices)){
        name<-getAssetName() #pick the last name
      }
      if(is.null(name) || !(name %in% choices)){
        name=choices[1] #pick the first alternative
      }
      selectedAsset$name=name
      if(is.null(tibs) ){
        selectedAsset$rowIndex=0
        selectedAsset$ptColName=NULL
        selectedAsset$columnName=NULL
        selectedAsset$matCol=NULL
      } else {
        tib<-getPtDefs()$tib[[selectedAsset$name]]
        # set row
        rowIndex=nrow( tib )
        selectedAsset$rowIndex=rowIndex
        # next we try to extract a pt column for the selected tib
        ptIndxs<-sapply(seq_along(names(tib)),function(j) is.matrix(tib[[rowIndex,j]]) && dim(tib[[rowIndex,j]])[1]==2)
        ptIndxs<-which(ptIndxs==TRUE)
        if(length(ptIndxs)>0){
          ptColNames<-names(tib)[ptIndxs]
          if(!is.null(selectedAsset$columnName) && selectedAsset$columnName %in% ptColNames){
            ptColName<-selectedAsset$columnName
          } else {
            ptColName<-head(ptColNames,1)
          }
          entry<-tib[[ptColName]][[rowIndex]]
          matColIndex<-ncol(entry)
          selectedAsset$matCol<-matColIndex
          # cat('ptColName=',format(ptColName),"\n")
          selectedAsset$ptColName=ptColName 
          if(is.null(selectedAsset$selIndex) || selectedAsset$selIndex!=2){
            #unless selected is 'matrix', set to 'point' 
            updateSelected( selIndex=1)
          }
        } else {
          ptColName<-NULL
          matColIndex<-0
          selectedAsset$ptColName=ptColName 
          
        }
         if(is.null(selectedAsset$columnName) || !( selectedAsset$columnName %in% names(tib) )){
          if(!is.null(selectedAsset$ptColName)){
            selectedAsset$columnName<-selectedAsset$ptColName
          } else {
            selectedAsset$columnName=names(tib[1])
          }
        }
      }
      if( selectedAsset$name==transformTag){
        selectedAsset$transformType='translate'
      }
      cat('<---f-< resetSelectedTibbleName\n')
}


setSelectedAssetFromAce<-function( reqSelector){
  # updatEle<- c('name', 'ptColName', 'rowIndex', 'matCol', 'colName')
  #cat('names of reqSelector:', paste(names(reqSelector), collapse=", "),"\n" )
  for(n in names(reqSelector)){
    stopifnot({n %in% names(selectedAsset)})
    selectedAsset[[n]]<-reqSelector[[n]]
  }
}

updateSelected<-function( name, rowIndex, columnName, matCol,  ptColName, selIndex, transformType ){
  if(!missing(name)){
    selectedAsset$name=name
  }
  if(!missing(ptColName)){
    selectedAsset$ptColName=ptColName
  }
  if(!missing(rowIndex)){ # !!! may want to provide a check here
    selectedAsset$rowIndex=rowIndex
  }
  if(!missing(matCol)){
    selectedAsset$matCol=matCol
  }
  if(!missing(selIndex)){
    selectedAsset$selIndex=selIndex
  }
  if(!missing(columnName)){
    selectedAsset$columnName=columnName
    if(!is.null(getColumnType()) && getColumnType()=='point'){
      selectedAsset$ptColName<-columnName
      if(!is.null(selectedAsset$row) && !is.null(columnName ) && !is.null(selectedAsset$name )){
        m<-getPtDefs()$tib[[ selectedAsset$name ]][[columnName]][[selectedAsset$row]]
        matCol<-selectedAsset$matCol
        # cat('matCol=',format(matCol),'\n')
        if(length(m>0)){
          matCol=min(matCol, ncol(m))
        } else {
          matCol=0
        }
      } else {
        matCol=0
      }
      # here is a candiate  location for updating the matCol???
    }
  }
  if(!missing(transformType)){
    selectedAsset$transformType=transformType
  }
} 

getTibColumnNameChoices<-reactive({
  tib<-getTib()
  choices<-tib %AND% names(tib)
  choices
})

getTibEntry<-reactive({
  # cat("serverSelection:: -----Entering-----getTibEntry::----------------\n")
  if( !is.null(getColumnType()) && getColumnType()=='point'){
    return( c('point','matrix')[getSelIndex()] )
  } 
  name<-getAssetName()
  rowNum<-getTibRow()
  columnName<-getTibColumnName()
  tib<-name %AND% getPtDefs()$tib[[name]]
  columnValues<- columnName %AND% tib[[columnName]]
  trows<-columnValues %AND% length(columnValues)
  entryOk<-trows %AND% rowNum %AND% (if(1<=rowNum && rowNum<=trows){ TRUE } else { NULL})
  if(!is.null(entryOk)){
     entry<- as.list(tib[[columnName]])[[rowNum]]
  } else {
    entry<-NULL
  }
  entry
})

getTibEntryChoices<-reactive({
  # cat("\n-----Entering-----getTibEntryChoices::----------------\n")
  if( !is.null(getColumnType()) && getColumnType()=='point'){
    return( c('point', 'matrix'))
  } 
  name<-getAssetName()
  columnName<-getTibColumnName()
  tib<-name %AND% getPtDefs()$tib[[name]]
  columnValues<- columnName %AND% tib[[columnName]]

  columnValues <-columnValues %AND%  as.list(columnValues)
  columnValues
})


getTibPts<-reactive({ 
  # cat("serverSelection:: -----Entering-----getTibPts::----------------\n")
  ptCol<-selectedAsset$ptColName
  tib<-getTib()
  pts <- tib %AND% ptCol %AND% tib[[ptCol]]
  pts
})


# todo refactor to return only last (or a pair)
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




