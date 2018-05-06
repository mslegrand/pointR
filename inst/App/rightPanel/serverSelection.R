
# todo!!! rename selectedTibble to something else, like currentPlotSelect 
# add svg as a name when needed.

selectedTibble <- reactiveValues(
  tabId="whatthefuck",
  name=NULL,        # name of current point array
  rowIndex=1,
  columnName=NULL, # currently used only by tibbleEditor and could be placed there.
  matCol=0, #
  ptColName=NULL,      # !!! KLUDGE for now. should this default to last col?
  selIndex=1, # only used is to determine if in matrix or point mode !! 
  transformType='Translate' # TODO!!! replace this with selIndex
)


getSelIndex<-reactive({
  selectedTibble$selIndex
})

observeEvent(getTibNRow(),{
  sendPtRManagerMessage( id= getAceEditorId() , sender='tibNrow', rowCountChange=TRUE)
})

getTibName<-reactive({selectedTibble$name}) #allow to be null only if tib is null  
getTibTabId<-reactive({ selectedTibble$tabId})
getTibColumnName<-reactive({ selectedTibble$columnName })
getTib<-reactive({ getTibName() %AND% getPtDefs()$tib[[ getTibName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})
getTibNRow<-reactive({
  if( getTibEditState()==TRUE ){
    nrow(getPtDefs()$tib[[getTibName()]])
  } else {
    0
  }
})

atLeast2Rows<-reactive({
  getTibEditState()==TRUE && nrow(getPtDefs()$tib[[getTibName()]])>1
})

getTibRow<-reactive({
  # cat('getTibRow()=',format( selectedTibble$rowIndex),"\n");
  selectedTibble$rowIndex})

getTibMatCol<-reactive({ 
  # cat('getTibMatCol::', format(selectedTibble$matCol), "\n")
  selectedTibble$matCol 
})
getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )

getTransformType<-reactive({ 
  if(is.null(selectedTibble$transformType)){
    'Translate'
  } else {
    selectedTibble$transformType
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
    if(hasError()){
      return(NULL) # never change selection when in error state
    }
    if(is.null(tibs)){
      return(NULL)
    }
    # cat("\\n----------nserverSelection...Entering  resetSelectedTibbleName\n")
    # cat("\nresetSelectedTibbleName... name= ", format(name),"\n")
    # cat("resetSelectedTibbleName... names(tibs)=",format(names(tibs)),"\n")
      choices<-getRightPanelChoices()
      #cat("resetSelectedTibbleName:: choices=", paste(choices, collapse=", "),"\n")
      if(is.null(name) || !(name %in% choices)){
        name<-getTibName() #pick the last name
      }
      if(is.null(getTibName()) || !(getTibName() %in% choices)){
        name=choices[1] #pick the first alternative
      }
      selectedTibble$name=name
      if(is.null(tibs) ){
        # cat('THIS SHOULD NEVER EXEC\n')
        selectedTibble$rowIndex=NULL
        selectedTibble$ptColName=NULL
        selectedTibble$columnName=NULL
        selectedTibble$matCol=NULL
      } else {
        # cat("2: selectedTibble$name=",format(selectedTibble$name),"\n" )
        tib<-getPtDefs()$tib[[selectedTibble$name]]
        # cat(" 2: tib=", names(tib),"\n" )
        # set row
        rowIndex=nrow( tib )
        selectedTibble$rowIndex=rowIndex
        # cat('resetSelectedTibbleName:: rowIndex=',format(rowIndex),"\n")
        # next we try to extract a pt column for the selected tib
        ptIndxs<-sapply(seq_along(names(tib)),function(j) is.matrix(tib[[rowIndex,j]]) && dim(tib[[rowIndex,j]])[1]==2)
        ptIndxs<-which(ptIndxs==TRUE)
        if(length(ptIndxs)>0){
          ptColNames<-names(tib)[ptIndxs]
          #ptColIndex<-ptIndxs[1] # we arbritarily pick the first point col to select, and we use it to obtain matCol
          if(selectedTibble$columnName %in% ptColNames){
            ptColName<-selectedTibble$columnName
          } else {
            ptColName<-head(ptColNames,1)
          }
          entry<-tib[[ptColName]][[rowIndex]]
          matColIndex<-ncol(entry)
          selectedTibble$matCol<-matColIndex
          # cat('ptColName=',format(ptColName),"\n")
          selectedTibble$ptColName=ptColName 
          if(is.null(selectedTibble$selIndex) || selectedTibble$selIndex!=2){
            #unless selected is 'matrix', set to 'point' 
            updateSelected( selIndex=1)
          }
        } else {
          ptColName<-NULL
          matColIndex<-0
          selectedTibble$ptColName=ptColName 
          
        }
        # cat("selectedTibble$columnName=",format(selectedTibble$columnName), "\n")
        # cat("names(tib)=",format(names(tib)),"\n")
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
      # cat("resetSelectedTibbleName... Last values\n")
      # cat("resetSelectedTibbleName... selectedTibble$name= ",selectedTibble$name,"\n")
      # cat("\n----------Exiting resetSelectedTibbleName... \n\n")
}

updateSelected<-function( name, rowIndex, columnName, matCol,  ptColName, selIndex, transformType ){
  if(!missing(name)){
    #cat(" updateSelected name=", format(name),"\n")
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
    selectedTibble$ptColName=ptColName
  }
  if(!missing(rowIndex)){ # !!! may want to provide a check here
    #cat('updateSelected:: rowIndex=',format(rowIndex),"\n")
    selectedTibble$rowIndex=rowIndex
  }
  if(!missing(matCol)){
    selectedTibble$matCol=matCol
  }
  if(!missing(selIndex)){
    selectedTibble$selIndex=selIndex
  }
  if(!missing(columnName)){
    selectedTibble$columnName=columnName
    if(!is.null(getColumnType()) && getColumnType()=='point'){
      selectedTibble$ptColName<-columnName
      #browser()
      # cat('selectedTibble$name=',format(selectedTibble$name),"\n")
      #browser()
      # cat('columnName=',format(columnName),"\n")
      #browser()
      # cat('selectedTibble$row=',format(selectedTibble$row),"\n")
      #browser()
      # print(getPtDefs()$tib[[ selectedTibble$name ]])
      if(!is.null(selectedTibble$row) && !is.null(columnName ) && !is.null(selectedTibble$name )){
        m<-getPtDefs()$tib[[ selectedTibble$name ]][[columnName]][[selectedTibble$row]]
        matCol<-selectedTibble$matCol
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

  columnValues <-columnValues %AND%  as.list(columnValues)
  columnValues
})


getTibPts<-reactive({ 
  # cat("serverSelection:: -----Entering-----getTibPts::----------------\n")
  # cat("selectedTibble$ptColName=",format(selectedTibble$ptColName),"\n")
  ptCol<-selectedTibble$ptColName
  tib<-getTib()
  # cat('names of tib', format(paste(names(tib))),"\n")
  pts <- tib %AND% ptCol %AND% tib[[ptCol]]
  # cat('length of pts=',length(pts),"\n")
  # cat("serverSelection:: -----leaving-----getTibPts::----------------\n\n")
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




