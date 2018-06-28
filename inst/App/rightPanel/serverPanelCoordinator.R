

panels<-reactiveValues(
  #left='source' ,   #to be used as editor name later, for connecting to right graphics
  #  sourceType can be either svgPanelTag or RPanelTag
  #  sourceType = 'svgPanelTag'  means svgR code
  #  sourceType = 'RPanelTag' means plain R code or error
  #  sourceType is set from processCommit  
  sourceType=svgPanelTag 
)

setSourceType<-function( sourceType ){
  # cat('setting sourceType to ',format(sourceType),"\n")
  panels$sourceType=sourceType
}

getSourceType<-reactive({ 
  panels$sourceType
})

# Returns a type corresp to name found in selectedTibble: 
# RPanelTag if it is to be RCode
# 'tib' if it is the name of an existing tibble
#  otherwise
getNameType<-reactive({
  # cat("getNameType::getTibName()=", format(getTibName()),"\n")
  if(hasError()){
    # cat('getNameType:: Error=', getErrorMssg(),"\n")
    errorPanelTag
  } else {
    #browser()
    if(!is.null(getTibName())){
      # print("names(getPtDefs()$tib:\n")
      # print(names(getPtDefs()$tib))
      # print(getTibName())
      if( getTibName() %in% names(getPtDefs()$tib) ){
        tibTag
      } else if(getTibName()==transformTag && usingTransformDraggable()) { 
        # return transformTag if transformTag and usingTransformDraggable()
        transformTag
      } else {
        getTibName()
      }
    } else { # RPanelTag whenever getTibName is NULL???
      RPanelTag
    }
  }
})

#returns the type of column, which can be 'point', 'list', 'numeric', 'colourable', 'value'
# would like to extend: list-numeric-pairs, list-character, 'numeric-int', 'numeric-pos', 'numeric-real',
# 'numeric-range'
# currently we only use getColumnType in 
#   1. getPlotState
#   2. undateSelected
#   3. getTibEntry, getTibEntryChoices
# and use it only for whether or not the column is a 'points' column.
getColumnType<-reactive({
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  if(!is.null(columnValues)){
    return(extractColType(columnValues))
  }
  return(NULL)
})

# returns the state: 'point', 'matrix', 'value',  transformTag, RPanelTag, errorPanelTag
# used by 
#    getTibEditState
#    getRightMidPanel
#    serverEdTib to set transform panel
getPlotState<-reactive({
  nameType<-getNameType()
  if(identical(nameType,tibTag)){
    colType<-getColumnType()
    if(identical(colType,'point')){
      c('point', 'matrix')[ getSelIndex() ]
    } else {
      'value'
    }
  } else {
    nameType
  }
})

# returns true iff editing tib contents
getTibEditState<-reactive({
  #cat("getTibEditState::getPlotState()=",format(getPlotState()),"\n")
  (panels$sourceType)==svgPanelTag && 
    !is.null(getPlotState()) && 
    getPlotState() %in%  c("point", "value", "matrix")
})

# used  by
# server.R:: ptrDisplayScript
# serverPanelDispatch::
# serverFooterRight.R
# serverMouseClicks.R, (as barName)
# serverLog.R (as barName)
# returns:
#  RPanelTag, rmdPanelTag, or oneof point, matrix, value, if ptR
getRightMidPanel<-reactive({
  cat('panels$sourceType=', panels$sourceType,"\n")
  if(hasError()){
    # cat('getRightMidPanel:: Error=', getErrorMssg(),"\n")
    rtv<-errorPanelTag
  } else if (panels$sourceType %in% c( RPanelTag, rmdPanelTag, textPanelTag, snippetPanelTag)){
    rtv<-panels$sourceType
  } else {
    rtv<-getPlotState()
  }
    
    
  #   if (panels$sourceType==RPanelTag){
  #   rtv<-RPanelTag
  # } else if (panels$sourceType==rmdPanelTag){
  #   rtv<-rmdPanelTag
  # } else if (panels$sourceType==textPanelTag){
  #   rtv<-textPanelTag
  # } else if (panels$sourceType==snippetPanelTag){
  #   rtv<-snippetPanelTag
  # } else {
  #    rtv<-getPlotState()
  # }
  rtv
})



getRightPanelChoices<-reactive({ # includes names of tibs
  # cat('getRightPanelChoices', format(getSourceType()),"\n")
  if(hasError()){ # error: set to  errorPanel
    choices<-errorPanelTag
  } else if( getSourceType()==RPanelTag){
    choices=RPanelTag
  } else if( getSourceType()==rmdPanelTag){
    choices=c( rmdPanelTag, RPanelTag )
  } else if( getSourceType()==snippetPanelTag){
    choices=snippetPanelTag
  } else { # ptRPanel (names of ptDefs not null), svgRPanel, 
    ptDefs<-getPtDefs()
    choices<-names(getPtDefs()$tib)
    # cat('getRightPanelChoices 1:: ', format(choices),"\n")
    if( usingTransformDraggable() ){
      choices<-c(choices, transformTag)
    }
    choices<-c(choices, svgPanelTag, RPanelTag)
    # cat('getRightPanelChoices 2:: ', format(choices),"\n")
  } 
  # cat('getRightPanelChoices 3:: ', format(choices),"\n")
  choices
})



is.tibName<-function(x){ !is.null(x) || x==errorPanelTag || x==transformTag}


usingTransformDraggable<-reactive({
  length(getCode()) >0 &&
    nchar(getCode())>0 &&
    ( 
      grepl("class\\s*=\\s*'draggable'",getCode() ) || 
        grepl('class\\s*=\\s*"draggable"',getCode() )
    )
}) 

observeEvent(atLeast2Rows(),{
  if(atLeast2Rows()){
    #show row, shrink display
    showElement('rowOutPanel')
    addCssClass('svgOutPanel', 'cSvgOutLeftIndent')
  } else {
    #hide rows, expand display
    hideElement('rowOutPanel')
    removeCssClass('svgOutPanel', 'cSvgOutLeftIndent')
  }
})

observeEvent(c(getAceEditorId(), getMode()),{
  id<-getAceEditorId()
  # cat("\nobserveEvent getAceEditorId:: id='",format(id),"'\n");
  # cat("\nobserveEvent getAceEditorId:: class(id)='",class(id),"'\n");
  # cat("\nobserveEvent getAceEditorId:: nchar(id)='",nchar(id),"'\n" )
  
  if(length(id)==0){
    #cat("hiding TopRightPanel\n")
    hideElement("TopRightPanel")
    hideElement("snippetToolBarContainer")
    hideElement("aceToobarTop1")
    hideElement("aceToobarTop2")
    hideElement("useTribble")
    hideElement("commit")
    hideElement("aceTabSet")
    hideElement("midRightPanels")
    hideElement("BottomRightPanel")
    showElement("logo.right")
    showElement("logo.left")
  } else {
    #cat("showinging TopRightPanel\n")
    hideElement("logo.right")
    hideElement("logo.left")
    
    if(identical(request$mode,'ptr')){
      showElement("BottomRightPanel")
      showElement("TopRightPanel")
      showElement("snippetToolBarContainer")
      showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      addClass( id= 'midRightPanels', class='ctop140')
    } else {
      removeClass( id= 'midRightPanels', class='ctop140')
      hideElement("TopRightPanel")
      hideElement("snippetToolBarContainer")
      hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
    }
    showElement("aceToobarTop1")
    showElement("aceToobarTop2")
    showElement("commit")
    showElement("aceTabSet")
    showElement("midRightPanels")
  }
  processCommit()
  
  
  #cat('name=',format(name),"\n")
  # tibs<-getPtDefs()$tib
  # resetSelectedTibbleName(tibs=tibs, name=NULL)
})

