
# todo!!! rename selectedTibble to something else, like selection 
# add svg as a name when needed.

selectedTibble <- reactiveValues(
  name=NULL,        # name of current point array
  rowIndex=1,
  columnName=NULL, # currently used only by tibbleEditor and could be placed there.
  matCol=0,
  ptColName=NULL,      # !!! KLUDGE for now. should this default to last col?
  selIndex=1,
  transformType='Translate' # TODO!!! replace this with selIndex
)


getSelIndex<-reactive({selectedTibble$selIndex})
getTibName<-reactive({selectedTibble$name}) #allow to be null only if tib is null  
getTibColumnName<-reactive({ selectedTibble$columnName })
getTib<-reactive({ getTibName() %AND% getPtDefs()$tib[[ getTibName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})
getTibNRow<-reactive({nrow(getPtDefs()$tib[[getTibName()]])})
getTibRow<-reactive({selectedTibble$rowIndex})
getTibMatCol<-reactive({ selectedTibble$matCol })
getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )

getTransformType<-reactive({ 
  if(is.null(selectedTibble$transformType)){
    'Translate'
  } else {
    selectedTibble$transformType
  }
})

# getTibMatColMax<-reactive({ 
#   rowNum<-getTibRow()
#   colMax<-getTibPtsNCol()
#   if(is.null(pts) || is.null(rowNum) || rowNum<1 || rowNum>length(colMax)){
#     NULL
#   } else {
#     ncol(pts[[rowNum]])
#   }
# })

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
  # cat("serverSelection...Entering  resetSelectedTibbleName\n")
      choices<-getRightPanelChoices()
      if(is.null(getTibName()) || !(getTibName() %in% choices)){
        selectedTibble$name=choices[1]
      }
      if(is.null(tibs) ){
        selectedTibble$rowIndex=NULL
        selectedTibble$ptColName=NULL
        selectedTibble$columnName=NULL
        selectedTibble$matCol=NULL
      } else {
        tib<-getPtDefs()$tib[[selectedTibble$name]]
        # set row
        rowIndex=nrow( tib )
        selectedTibble$rowIndex=rowIndex
        # next we try to extract a pt column for the selected tib
        ptIndxs<-sapply(seq_along(names(tib)),function(j) is.matrix(tib[[rowIndex,j]]) && dim(tib[[rowIndex,j]])[1]==2)
        ptIndxs<-which(ptIndxs==T)
        if(length(ptIndxs)>0){
          ptColIndex<-ptIndxs[1]
          entry<-tib[[rowIndex,ptColIndex]]
          matColIndex<-ncol(entry)
          selectedTibble$matCol<-matColIndex
          ptColName<- names(tib)[ptColIndex]
          selectedTibble$ptColName=ptColName 
          #selectedTibble$columnName=ptColName #this is the problem!!! should not reset if newColumn,
          if(is.null(selectedTibble$selIndex) || selectedTibble$selIndex!=2){
            updateSelected( selIndex=1)
          }
        } else {
          ptColName<-NULL
          matColIndex<-0
          selectedTibble$ptColName=ptColName 
          #selectedTibble$columnName=names(tib[1])
        }
        if(is.null(selectedTibble$columnName) || !( selectedTibble$columnName %in% names(tib) )){
          if(!is.null(selectedTibble$ptColName)){
            selectedTibble$columnName<-selectedTibble$ptColName
          } else {
            selectedTibble$columnName=names(tib[1])
          }
        }
      }
      if( selectedTibble$name==transformTag){
        selectedTibble$transformType='translate'
      }
        
      # if(is.null(tibs) || is.null(names(tibs)) || length(names(tibs))==0){ 
      #   selectedTibble$rowIndex=NULL
      #   selectedTibble$ptColName=NULL
      #   selectedTibble$columnName=NULL
      #   selectedTibble$matCol=NULL
      #   selectedTibble$transformType=NULL
      #   if(usingTransformDraggable()){
      #     selectedTibble$name=transformTag
      #     selectedTibble$transformType='translate'
      #   } else {
      #     selectedTibble$name=logTag
      #   }
      #   return(NULL)
      #}
  
  # if(is.null(name) || nchar(name)==0 || !(name %in% names(tibs))){ 
  #   name=names(tibs)[1]
  # }
  # selectedTibble$name=name
  # tib<-tibs[[name]]
  
  
}

updateSelected<-function( name, rowIndex, columnName, matCol,  ptColName, selIndex, transformType ){
  if(!missing(name)){
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
    selectedTibble$ptColName=ptColName
  }
  if(!missing(rowIndex)){ # !!! may want to provide a check here
    selectedTibble$row=rowIndex
  }
  if(!missing(matCol)){
    selectedTibble$matCol=matCol
  }
  if(!missing(selIndex)){
    selectedTibble$selIndex=selIndex
  }
  if(!missing(columnName)){
    # cat('setting columnName to ', columnName,"\n")
    selectedTibble$columnName=columnName
    if(!is.null(getColumnType()) && getColumnType()=='point'){
      selectedTibble$ptColName<-columnName
    }
  }
  if(!missing(transformType)){
    selectedTibble$transformType=transformType
  }
} 

getTibColumnNameChoices<-reactive({
  tib<-getTib()
  choices<-tib %AND% names(tib)
  choices
})

getTibEntry<-reactive({
  if( !is.null(getColumnType()) && getColumnType()=='point'){
    return( c('point','matrix')[getSelIndex()] )
  } 
  name<-getTibName()
  rowNum<-getTibRow()
  columnName<-getTibColumnName()
  # cat("serverSelection:: -----Entering-----getTibEntry::----------------\n")
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
  name<-getTibName()
  columnName<-getTibColumnName()
  tib<-name %AND% getPtDefs()$tib[[name]]
  columnValues<- columnName %AND% tib[[columnName]]

  columnValues <-columnValues %AND% as.list(columnValues)
  columnValues
})


getTibPts<-reactive({ 
  ptCol<-selectedTibble$ptColName
  tib<-getTib()
  pts <- tib %AND% ptCol %AND% tib[[ptCol]]
  pts
})



getTibMatColChoices<-reactive({ 
  rowNum<-getTibRow()
  pts<-getTibPts()
  #cat('rowNum=',rowNum,'\n')
  #print(pts)
  #cat('length(pts)=',length(pts),"\n")
  if(is.null(pts) || is.null(rowNum) || rowNum<1 || rowNum>length(pts)){
    rtv<-NULL
  } else {1
    mc<-ncol(pts[[rowNum]])
    # cat('length(mc)=',length(mc),'\n')
    # cat('mc=',mc,'\n')
    if(mc>0){
      # cat('mc=',mc,'\n')
      rtv<-1:mc
    } else {
      # cat('mc=0\n')
      rtv<-0
    }
  }
  rtv
})


# getTibColumn<-reactive({
#   colName<-getTibColumnName()
#   if(!is.null(colName) && nchar(colName)>0){
#     columnNameChoices=getTibColumnNameChoices()
#     ptPos<-getTibPtColPos()
#     column<-match(colName, columnNameChoices, nomatch=ptPos)
#   } else {
#     column<-NULL
#   }
#   column
# })


