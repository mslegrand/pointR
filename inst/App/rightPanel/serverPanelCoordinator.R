

panels<-reactiveValues(
  #left='source' ,   #to be used as editor name later, for connecting to right graphics
  #  sourceType can be either svgPanelTag or RPanelTag
  #  sourceType = 'svgPanelTag'  means svgR code
  #  sourceType = 'RPanelTag' means plain R code or error
  #  sourceType is set from processCommit  
  sourceType=textPanelTag 
)

observeEvent( getSourceType(),{
  if(identical(getSourceType(), svgPanelTag)){
    enableDMDM(session, "editNavBar", "Export as SVG")
  } else {
    disableDMDM(session, "editNavBar", "Export as SVG")
  }
})

setSourceType<-function( sourceType ){
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
  # cat('>---> getNameType\n')
  if(hasError()){
    rtv<-errorPanelTag
  } else {
    if(!is.null(getAssetName())){
      # browser()
      if( getAssetName() %in% names(getPtDefs()$tib) ){
        rtv<-tibTag
      } else if( identical(getAssetName(),transformTag) && usingTransformDraggable()) { 
        # return transformTag if transformTag and usingTransformDraggable()
        rtv<-transformTag
      } else {
        #rtv<-RPanelTag 
        rtv<-getAssetName()
      }
    } else { # RPanelTag whenever getAssetName is NULL???
      rtv<-RPanelTag
    }
    # cat('rtv=',format(rtv),"\n")
    # cat("<---< getNameType\n")
  }
  rtv
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
  # cat(">----> getPlotState\n")
  nameType<-getNameType()
  if(identical(nameType,tibTag)){
    colType<-getColumnType()
    if(identical(colType,'point')){
      rtv<-c('point', 'matrix')[ getSelIndex() ]
    } else {
      rtv<-'value'
    }
  } else {
    rtv<-nameType
  }
  # cat("<----< getPlotState\n")
  rtv
})

# returns true iff editing tib contents
getTibEditState<-reactive({
  # cat("getTibEditState::getPlotState()=",format(getPlotState()),"\n")
  # cat(">----> getTibEditState\n")
  rtv<-getSourceType()==svgPanelTag && 
    !is.null(getPlotState()) && 
    getPlotState() %in%  c("point", "value", "matrix")
  # cat("<----< getTibEditState\n")
  rtv
}
)

# used  by
# server.R:: ptrDisplayScript
# serverPanelDispatch::
# serverFooterRight.R
# serverMouseClicks.R, (as barName)
# serverLog.R (as barName)
# returns:
#  RPanelTag, rmdPanelTag, or oneof point, matrix, value, if ptR
getRightMidPanel<-reactive({
  # cat(">---> getRightMidPanel\n")
  if(hasError()){
    rtv<-errorPanelTag
  } else if (panels$sourceType %in% c( rmdPanelTag, textPanelTag, snippetPanelTag, javascriptPanelTag, appPanelTag) ){
    rtv<-panels$sourceType
  } else {
    rtv<-getPlotState()
  }
  # cat('getRightMidPanel=',format(rtv),'\n')
  # cat("<---< getRightMidPanel\n")
  rtv
})



getRightPanelChoices<-reactive({ # includes names of tibs
  if(hasError() ){ # error: set to  errorPanel
    choices<-errorPanelTag
  } else {
    sourceType<-getSourceType()
    if( identical(sourceType,RPanelTag) ){
      choices=RPanelTag
    } else if( identical(sourceType,appPanelTag) ){
      choices=appPanelTag
    } else if( identical(sourceType, rmdPanelTag) ){
      choices=c( rmdPanelTag, RPanelTag )
    } else if( identical(sourceType, snippetPanelTag ) ){
      choices=snippetPanelTag
    } else if( getSourceType()==svgPanelTag){ 
      ptDefs<-getPtDefs()
      choices<-names(getPtDefs()$tib)
      if( usingTransformDraggable() ){
        choices<-c(choices, transformTag)
      } 
      choices<-c(choices, svgPanelTag, RPanelTag)
    } else{
      choices<-textPanelTag
    }
  } 
  choices
},
label= 'getRightPanelChoices'
)


observeEvent(c(getSourceType(), hasError()),{
  if(!hasError() && identical(getSourceType(), svgPanelTag)){
    enableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Grid"
    )
    enableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Backdrop"
    )
  } else {
    disableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Grid"
    )
    disableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Backdrop"
    )
  }
}, label='ShowGridMenu')


observeEvent(c(getSourceType(), hasError(), getPtDefs() ),{
  if(
    !hasError() && 
    identical(getSourceType(), svgPanelTag) &&
    length(names(getPtDefs()$tib))>0
  ){
    enableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Points"
    )
  } else {
    disableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Points"
    )
  }
}, label='ShowGridMenu')



is.tibName<-function(x){ !is.null(x) || x==errorPanelTag || x==transformTag}


usingTransformDraggable<-reactive({
  code<-getCode()
  length(code) >0 &&
    nchar(code)>0 &&
    ( 
      grepl("class\\s*=\\s*'draggable'",code ) || 
        grepl('class\\s*=\\s*"draggable"',code )
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



observeEvent( getRightMidPanel(), {
  panel<-getRightMidPanel()
  menuBarId="plotNavBar"
  entry="Attribute Preprocessor"
  if( identical(panel,'value')){
    enableDMDM(session, menuBarId=menuBarId, entry=entry)
  } else {
    disableDMDM(session, menuBarId=menuBarId, entry=entry)
  }
  entry="Point Preprocessor"
  if( identical(panel,'point') || identical(panel,'matrix')){
    enableDMDM(session, menuBarId=menuBarId, entry=entry)
  } else {
    disableDMDM(session, menuBarId=menuBarId, entry=entry)
  }
}, label='getRightMidPanel')

# need to rewrite for cmdExportAP cmdRemoveAP
observeEvent( c( getRightMidPanel(), hasPreProcChoices() ), {
  if( !is.null(getRightMidPanel()) && 
      #(getRightMidPanel() %in% c('point','matrix')) &&
      hasPreProcChoices() 
  ){
    removeCssClass( id='PtPreProcDiv', class="hiddenPanel")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  } else {
    addCssClass( id='PtPreProcDiv', class="hiddenPanel")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  }
}, 
label='PanelCoordinator.R:: c(getRightMidPanel(), hasPreProcChoices())' 
)
