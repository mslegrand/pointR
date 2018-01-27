
# todo!!! rename selectedTibble to something else, like selection 
# add svg as a name when needed.
# selectedTibble <- reactiveValues(
#   name="x",        # name of current point array
#   row=1,
#   columnName='x', # currently used only by tibbleEditor and could be placed there.
#   matCol=0,
#   ptColName='x',      # !!! KLUDGE for now. should this default to last col?
#   transformType='Translate'
# )

selectedTibble <- reactiveValues(
  name=NULL,        # name of current point array
  rowIndex=1,
  columnName=NULL, # currently used only by tibbleEditor and could be placed there.
  matCol=0,
  ptColName=NULL,      # !!! KLUDGE for now. should this default to last col?
  selIndex=1,
  transformType='Translate' #replace this with selIndex
)

getSelIndex<-reactive({selectedTibble$selIndex})

getTransformType<-reactive({ 
  if(is.null(selectedTibble$transformType)){
    selectedTibble$transformType ='Translate'
  }
  cat('getTransformType=',selectedTibble$transformType,"\n")
  selectedTibble$transformType
})


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
  cat("serverSelection...Entering  resetSelectedTibbleName\n")
  print(tibs)
  if(is.null(tibs) || is.null(names(tibs)) || length(names(tibs))==0){ 
    selectedTibble$rowIndex=NULL
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
    selectedTibble$columnName=ptColName
    #cat("serverSelection...  resetSelectedTibbleName  calling setPlotState('point')\n")
    #setPlotState('point')
    if(is.null(selectedTibble$selIndex) || selectedTibble$selIndex!=2){
       updateSelected( selIndex=1)
    }
  } else {
    ptColName<-NULL
    matColIndex<-0
    selectedTibble$ptColName=ptColName 
    selectedTibble$columnName=names(tib[1])
    #cat("serverSelection...  resetSelectedTibbleName  calling setPlotState('value')\n")
    #setPlotState('value')
  }
  
}

updateSelected<-function( name, rowIndex, columnName, matCol,  ptColName, selIndex ){
  # triggerRefresh(sender='update.tibEd', rollBack=FALSE, auxValue=FALSE)
  if(!missing(name)){
    #cat("updateSelected::name=",name,"\n")
    selectedTibble$name=name
  }
  if(!missing(ptColName)){
    #cat("updateSelected::ptColName=",ptColName,"\n")
    selectedTibble$ptColName=ptColName
  }
  if(!missing(rowIndex)){ # !!! may want to provide a check here
    #cat("updateSelected::row=",row,"\n")
    selectedTibble$row=rowIndex
  }
  if(!missing(matCol)){
    # #cat("updateSelected::matCol=",matCol,"\n")
    # if(matCol=='end'){
    #   mc<-ncol(getTibPts()[[selectedTibble$tibSel$row]])
    #   #selectedTibble$tibSel$matCol = ifelse(is.integer(mc), mc, 0)
    #   tmp$selTib$matCol== ifelse(is.integer(mc), mc, 0)
    # } else {
    selectedTibble$matCol=matCol
    #}
  }
  if(!missing(selIndex)){
    selectedTibble$selIndex=selIndex
  }
  if(!missing(columnName)){
    #cat("updateSelected::columnName=",columnName,"\n")
    selectedTibble$columnName=columnName
    if(!is.null(getColumnType()) && getColumnType()=='point'){
      selectedTibble$ptColName<-columnName
    }
  }
} 


# updateSelected<-function( name, row, columnName, matCol,  ptColName ){
#  # triggerRefresh(sender='update.tibEd', rollBack=FALSE, auxValue=FALSE)
#   tmp<-list( name= NULL, selTib=list())
#   tibs<-getPtDefs()$tib
#   if(!missing(name)){
#     #cat("updateSelected::name=",name,"\n")
#     #selectedTibble$name=name
#     tmp$name=name
#     selTib$name=name
#   }
#   if(!missing(ptColName)){
#     #cat("updateSelected::ptColName=",ptColName,"\n")
#     #selectedTibble$tibSel$ptColName=ptColName
#     tmp$selTib$ptColName=ptColName
#   }
#   if(!missing(row)){ # !!! may want to provide a check here
#     #cat("updateSelected::row=",row,"\n")
#     #selectedTibble$tibSel$row=row
#     tmp$selTib$rowIndex=row
#   }
#   if(!missing(matCol)){
#     # #cat("updateSelected::matCol=",matCol,"\n")
#     # if(matCol=='end'){
#     #   mc<-ncol(getTibPts()[[selectedTibble$tibSel$row]])
#     #   #selectedTibble$tibSel$matCol = ifelse(is.integer(mc), mc, 0)
#     #   tmp$selTib$matCol== ifelse(is.integer(mc), mc, 0)
#     # } else {
#       selectedTibble$tibSel$matCol=matCol
#       tmp$selTib$matCol==matCol
#     #}
#   }
#   
#   if(!missing(columnName)){
#     #cat("updateSelected::columnName=",columnName,"\n")
#     #selectedTibble$tibSel$columnName=columnName
#     tmp$selTib$columnName=columnName
#   }
#   control$selector<-selectorUpdate( tibs, tmp, control$selector )
# } 


getTibName<-reactive({selectedTibble$name}) #allow to be null only if tib is null  


getTibColumnName<-reactive({
  selectedTibble$columnName
})

getTibColumnNameChoices<-reactive({
  tib<-getTib()
  choices<-tib %AND% names(tib)
  choices
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

getTibEntry<-reactive({
  if( !is.null(getColumnType()) && getColumnType()=='point'){
    return( c('point','matrix')[getSelIndex()] )
  } 
  name<-getTibName()
  rowNum<-getTibRow()
  columnName<-getTibColumnName()
  cat("serverSelection:: -----Entering-----getTibEntry::----------------\n")
  tib<-name %AND% getPtDefs()$tib[[name]]
  cat("serverSelection:: names(tib)=",names(tib),"\n")
  columnValues<- columnName %AND% tib[[columnName]]

  trows<-columnValues %AND% length(columnValues)
  cat("serverSelection:: columnName",columnName,"\n")
  cat("serverSelection:: class(columnValues)=",class(columnValues),"\n")
  entryOk<-trows %AND% rowNum %AND% (if(1<=rowNum && rowNum<=trows){ TRUE } else { NULL})
  if(!is.null(entryOk)){
     entry<- as.list(tib[[columnName]])[[rowNum]]
  } else {
    entry<-NULL
  }
  if(is.null(entry)){
    cat('serverSelection:: tibEntry is NULL\n')
  }
  else if(is.matrix(entry)){
    cat('tibEntry is matrix\n')
  }
  else if( is.character(entry)){
    cat('serverSelection:: tibEntry=',entry,'\n')
  }
  cat("serverSelection:: -----Exiting-----getTibEntry::----------------\n")
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


# getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )


getTibNRow<-reactive({nrow(getPtDefs()$tib[[getTibName()]])})


getTibRow<-reactive({selectedTibble$rowIndex})



getTibMatCol<-reactive({ 
  #cat( "selectedTibble$matCol=", selectedTibble$matCol ,"\n" )
  selectedTibble$matCol 
})

# getTibMatCol<-reactive({ 
#   #cat( "selectedTibble$matCol=", selectedTibble$matCol ,"\n" )
#   selectedTibble$tibSel$matCol 
# })
# 


getTibMatColChoices<-reactive({ 
  rowNum<-getTibRow()
  pts<-getTibPts()
  cat('rowNum=',rowNum,'\n')
  print(pts)
  cat('length(pts)=',length(pts),"\n")
  if(is.null(pts) || is.null(rowNum) || rowNum<1 || rowNum>length(pts)){
    rtv<-NULL
  } else {1
    mc<-ncol(pts[[rowNum]])
    cat('length(mc)=',length(mc),'\n')
    cat('mc=',mc,'\n')
    if(mc>0){
      cat('mc=',mc,'\n')
      rtv<-1:mc
    } else {
      cat('mc=0\n')
      rtv<-0
    }
  }
  rtv
})



