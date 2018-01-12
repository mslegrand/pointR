
# todo!!! rename selectedTibble to something else, like selection 
# add svg as a name when needed.
selectedTibble <- reactiveValues(
  name="x",        # name of current point array
  row=1,
  columnName='x', # currently used only by tibbleEditor and could be placed there.
  matCol=0,
  ptColName='x',      # !!! KLUDGE for now. should this default to last col?
  transformType='Translate'
)

getTransformType<-reactive({ 
  if(is.null(selectedTibble$transformType)){
    selectedTibble$transformType ='Translate'
  }
  cat('getTransformType=',selectedTibble$transformType,"\n")
  selectedTibble$transformType
})

getHandlerValue<-reactive({ 
  handler<-getHandler()
  if(is.null(handler)){ #NULL is default
    return(NULL)
  }
  name<-getTibName()
  columnName<-getTibColumnName()
  
  hv<-request$inputHandler[[name]][[columnName]]
  
  if( !is.null(hv) && hv==handler){
    return(handler)
  } else {
    return(NULL)
  }
})

getHandler<-reactive({
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  if(!is.null(columnValues)){
    if(is.character(columnValues) && isColorString(columnValues)){
      return('colourable')
    }
  }
  NULL
})

setHandlerValue<-function(hValue){ # hValue==NULL iff is 'default'
  handler<-getHandler()
  # if(handler=='default') bail
  if(is.null(handler)) {
    return(NULL)
  }
  if(!is.null(hValue) && hValue!=handler){
    return(NULL)
  }
  if(is.null(request$inputHandler)){
    request$inputHandle<-list()
  }
  name<-getTibName()
  colName<-getTibColumnName()
  # if tibName not in request$inputHandler list, add it with vector as arg
  if(is.null(request$inputHandler[[name]])){ 
      request$inputHandler[[name]]<- list()
  }
  request$inputHandler[[name]][[colName]]<-hValue
}

# observeEvent( getHandler(),{
#   hcs<-getHandler()
#   id<-"tagValBar"
#   ns <- NS(id)
#   if(hcs=='colourable'){
#     showElement(ns("useColourPalette"))
#   } else {
#     hideElement(ns("useColourPalette"))
#   }
# })
# 
# observeEvent( getHandlerValue(),{
#   hv<-getHandlerValue()
#   if(!is.null(hv)){
#       id<-"tagValBar"
#       ns <- NS(id)
#       if(hv=='colourable'){
#         showElement(ns("entryColour"))
#         showElement(ns("entryValue"))
#       } else {
#         showElement(ns("entryValue"))
#       }    
#   } else {
#     showElement(ns("entryValue"))
#   }
# 
# })

# # observer to update moduleTibEd for columntype
# observeEvent(getTibColumnName(),{
#   # get the columnValues
#   name<-getTibName()
#   if(!name %in% names(request$inputHandlers) ){
#     request$inputHandlers<-c(request$inputHandlers, list(name=c() ))
#   }
#   # if(!(colName %in% request$inputHandler[[name]])){
#   #   request$inputHandler[[name]]<-c( request$inputHandler[[name]], list(colName=list()))
#   # }
#   id<-"tagValBar"
#   ns <- NS(id)
#   colName<-getTibColumnName()
#   columnValues<-getTib()[[colName]]
#   if(!is.null(columnValues)){
#      if(is.character(columnValues) && isColorString(columnValues)){
#        showElement(ns("useColourPalette"))
#        #set to current value ???
#        # getValue from request$inputHandler
#        handler<-request$inputHandler[[name]][[colName]]
#        if( !is.null(handler) && handler=="useColourPalette"){
#          # set to use palette
#          selectedTibble$handler<-"useColourPalette"
#          showElement(ns("useColourPalette"))
#        } else {
#          # set to not use palette
#          selectedTibble$handler<-"donotUseColourPalette"
#          hideElement(ns("useColourPalette"))
#        }
#      } else {
#        hideElement(ns("useColourPalette"))
#        hideElement(ns("entryColour"))
#        showElement(ns("entryValue"))
#      }
#   }
# })
# 



# this is called to reset the tib name when possible 
# used by 
#    1. serverEdtib to reset the name when the selection changes
#    2. serveAce to reset name when we have a file->New or file->Open
resetSelectedTibbleName<-function(tibs, name){
  if(is.null(tibs) || is.null(names(tibs)) || length(names(tibs))==0){ 
    selectedTibble$row=NULL
    selectedTibble$ptColName=NULL
    selectedTibble$columnName=NULL
    selectedTibble$matCol=NULL
    selectedTibble$transformType=NULL
    if(usingTransformDraggable()){
      selectedTibble$name=transformTag
    } else {
      selectedTibble$name=logTag
    }
    return(NULL)
  }
  
  if(is.null(name) || nchar(name)==0 || !(name %in% names(tibs))){ 
    name=names(tibs)[1]
  }
  cat("\nresetSelectedTibbleName:: setting selectedTibble$name=",name,"\n")
  selectedTibble$name=name
  tib<-tibs[[name]]
  
  # set row
  rowIndex=nrow( tib )
  selectedTibble$row=rowIndex
  
 
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
    selectedTibble$columnName=ptColName
    setPlotState('point')
  } else {
    ptColName<-NULL
    matColIndex<-0
    selectedTibble$ptColName=ptColName 
    selectedTibble$columnName=names(tib[1])
    setPlotState('value')
  }
  
}

updateSelected<-function( name, row, columnName, matCol,  ptColName ){
 # triggerRefresh(sender='update.tibEd', rollBack=FALSE, auxValue=FALSE)
  if(!missing(name)){
    #cat("updateSelected::name=",name,"\n")
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
    #cat("updateSelected::ptColName=",ptColName,"\n")
    selectedTibble$ptColName=ptColName
  }
  if(!missing(row)){ # !!! may want to provide a check here
    #cat("updateSelected::row=",row,"\n")
    selectedTibble$row=row
  }
  if(!missing(matCol)){
    #cat("updateSelected::matCol=",matCol,"\n")
    if(matCol=='end'){
      mc<-ncol(getTibPts()[[selectedTibble$row]])
      selectedTibble$matCol = ifelse(is.integer(mc), mc, 0)
    } else {
      selectedTibble$matCol=matCol
    }
  }
  
  if(!missing(columnName)){
    #cat("updateSelected::columnName=",columnName,"\n")
    selectedTibble$columnName=columnName
  }
}

getCode<-reactive({request$code})
getTibName<-reactive({selectedTibble$name}) #allow to be null only if tib is null  


getTibColumnName<-reactive({
  selectedTibble$columnName
})

getTibColumnNameChoices<-reactive({ 
  tib<-getTib()
  choices<-tib %AND% names(tib)
  choices
})

getTibColumn<-reactive({
  colName<-getTibColumnName()
  if(!is.null(colName) && nchar(colName)>0){
    columnNameChoices=getTibColumnNameChoices()
    ptPos<-getTibPtColPos()
    column<-match(colName, columnNameChoices, nomatch=ptPos)
  } else {
    column<-NULL
  }
  column
})

getTibEntry<-reactive({
  name<-getTibName()
  rowNum<-getTibRow()
  columnName<-getTibColumnName()
  # cat("\n-----Entering-----getTibEntry::----------------\n")
  tib<-name %AND% getPtDefs()$tib[[name]]
  
  columnValues<- columnName %AND% tib[[columnName]]
  
  trows<-columnValues %AND% length(columnValues)
  
  entryOk<-trows %AND% rowNum %AND% (if(1<=rowNum && rowNum<=trows){ TRUE } else { NULL})
  if(!is.null(entryOk)){
     entry<- as.list(tib[[columnName]])[[rowNum]]
  } else {
    entry<-NULL
  }
   
  #cat("\n-----Exiting-----getTibEntry::----------------\n")
  entry
})

getTibEntryChoices<-reactive({
  # cat("\n-----Entering-----getTibEntryChoices::----------------\n")
  name<-getTibName()
  columnName<-getTibColumnName()
  tib<-name %AND% getPtDefs()$tib[[name]]
  columnValues<- columnName %AND% tib[[columnName]]   
  
  columnValues <-columnValues %AND% as.list(columnValues)
  # cat("\n-----Exiting-----getTibEntryChoices::----------------\n")
  columnValues
})

getTib<-reactive({ getTibName() %AND% getPtDefs()$tib[[ getTibName() ]] })
getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})

getTibPts<-reactive({ 
  ptCol<-selectedTibble$ptColName
  tib<-getTib()
  pts <- tib %AND% ptCol %AND% tib[[ptCol]]
  pts
})


getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )





getTibRow<-reactive({selectedTibble$row})
getTibRowChoices<-reactive({ 
  tib<-getTib()
  if(!is.null(tib) && is.finite(nrow(tib)) && nrow(tib)>0 ){
    1:nrow(tib) 
  } else {
    1
  }
})



getTibMatCol<-reactive({ 
  #cat( "selectedTibble$matCol=", selectedTibble$matCol ,"\n" )
  selectedTibble$matCol 
})



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



