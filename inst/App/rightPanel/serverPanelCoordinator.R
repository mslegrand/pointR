

panels<-reactiveValues(
  #left='source' ,   #to be used as editor name later, for connecting to right graphics
  #  sourceType can be either svgPanelTag or RPanelTag
  #  sourceType = 'svgPanelTag'  means svgR code
  #  sourceType = 'RPanelTag' means plain R code or error
  #  sourceType is set from processCommit  
  sourceType=svgPanelTag 
)

observeEvent( panels$sourceType,{
  if(identical(panels$sourceType, svgPanelTag)){
    enableDMDM(session, "editNavBar", "Export as SVG")
  } else {
    disableDMDM(session, "editNavBar", "Export as SVG")
  }
})

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
  if(hasError()){
    errorPanelTag
  } else {
    if(!is.null(getAssetName())){
      if( getAssetName() %in% names(getPtDefs()$tib) ){
        tibTag
      } else if(getAssetName()==transformTag && usingTransformDraggable()) { 
        # return transformTag if transformTag and usingTransformDraggable()
        transformTag
      } else {
        getAssetName()
      }
    } else { # RPanelTag whenever getAssetName is NULL???
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
  if(hasError()){
    rtv<-errorPanelTag
  } else if (panels$sourceType %in% c( RPanelTag, rmdPanelTag, textPanelTag, snippetPanelTag)){
    rtv<-panels$sourceType
  } else {
    rtv<-getPlotState()
  }
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
    if( usingTransformDraggable() ){
      choices<-c(choices, transformTag)
    }
    choices<-c(choices, svgPanelTag, RPanelTag)
  } 
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
    addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
    hideElement("aceTabSet")
    hideElement("midRightPanels")
    hideElement("BottomRightPanel")
    showElement("logo.right")
    showElement("logo.left")
  } else {
    #cat("showinging TopRightPanel\n")
    hideElement("logo.right")
    hideElement("logo.left")
    # editing ptr
    if(identical(request$mode,'ptr')){
      showElement("BottomRightPanel")
      showElement("TopRightPanel")
      showElement("snippetToolBarContainer")
      showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      addClass( id= 'midRightPanels', class='ctop140')
    } else { # editing other
      removeClass( id= 'midRightPanels', class='ctop140')
      hideElement("TopRightPanel")
      hideElement("snippetToolBarContainer")
      hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      if(identical(request$mode,'ptrrmd')){
        removeClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
      else{
        addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
    }
    showElement("aceToobarTop1")
    showElement("aceToobarTop2")
    showElement("commit")
    showElement("aceTabSet")
    showElement("midRightPanels")
  }
  processCommit()
})


observeEvent( c(getRightMidPanel(), hasPtScript()), {
  if( !is.null(getRightMidPanel()) && getRightMidPanel() %in% c('point','matrix')
      &&  hasPtScript() ){
    # cat('removing class hiddenPanel\n')
    addClass( id='rightFooterPointButtons', class='posl30b0')
    addClass( id='rightFooterMatrixButtons', class='posl30b0')    
    removeClass( id='PtPreProcDiv', class="hiddenPanel")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  } else {
    # cat('adding class hiddenPanel\n')
    addClass( id='PtPreProcDiv', class="hiddenPanel")
    removeClass( id='rightFooterPointButtons', class='posl30b0')
    removeClass( id='rightFooterMatrixButtons', class='posl30b0')
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  }
})
