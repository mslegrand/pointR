
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
  ptColName=NULL,      # !!! KLUDGE for now. should this default to last col? probably not
  selIndex=1, # only used is to determine if in matrix or point mode !! 
  transformType='Translate', # TODO!!! replace this with selIndex
  ptScriptSel=preprocChoices$points[1] #assigned but not used?
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
getTib<-reactive({ getPtDefs() %$$% 'tib' %$$%  getAssetName() })
getTibColPos<-reactive({ which(names(getTib())==selectedAsset$columnName )})
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

getTibRow<-reactive({selectedAsset$rowIndex})
getTibMatCol<-reactive({ selectedAsset$matCol })
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

#' this is called to reset the tib name when possible 
#' used in 2 spots 
#'    1. serverEdtib to reset the name when the selection changes
#'    2. **processMssgFromAceMssgPageIn** to handle senders
#'        cmd.commit
#'        cmd.add.column
#'        cmd.add.asset
#'
resetSelectedTibbleName<-function(tibs, name){
    # log.fin(resetSelectedTibbleName)
    if(hasError()){
      return(NULL) # never change selection when in error state
    }
      choices<-getRightPanelChoices()
      # cat("resetSelectedTibbleName:: choices=", paste(choices, collapse=", "),"\n")
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
            if(length(tib)>0){
              selectedAsset$columnName=tail(names(tib),1)
            } else {
              selectedAsset$columnName=NULL
            }
          }
        }
      }
      resetRowPickeR()
      if( selectedAsset$name==transformTag){
        selectedAsset$transformType='Translate'
      }
      # log.fout(resetSelectedTibbleName)
}


setSelectedAssetFromAce<-function( reqSelector){
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
    if(identical(getColumnType(), 'point')){
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
    }
  }
  if(!missing(transformType)){
    selectedAsset$transformType=transformType
  }
} 

getTibColumnNameChoices<-reactive({ names( getTib() ) })

getTibEntry<-reactive({
  if( identical(getColumnType(), 'point')){
    return( c('point','matrix')[getSelIndex()] )
  } 
  rowNum<-getTibRow()
  if(is.null(rowNum)){ return( NULL)}
  columnValues<-getTibEntryChoices()
  if(1<=rowNum && rowNum<=length(columnValues) ){
    entry<-columnValues[[rowNum]]
  } else {
    entry<-NULL
  }
  entry
})

getTibEntryChoices<-reactive({
  if( identical(getColumnType(), 'point')){
    return( c('point', 'matrix'))
  } 
  columnValues<-getTib() %$$%  getTibColumnName()
  if(!is.null(columnValues)){
    columnValues <-  as.list(columnValues)
  }
  columnValues
})


getTibPts<-reactive({getTib() %$$% selectedAsset$ptColName})

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




