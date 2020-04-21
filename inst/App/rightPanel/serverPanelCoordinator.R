

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
      if( getAssetName() %in% names(getPtDefs()$tib) ){
        rtv<-tibTag
      } else if( identical(getAssetName(),transformTag) && usingTransformDraggable()) { 
        # return transformTag if transformTag and usingTransformDraggable()
        rtv<-transformTag
      } else {
        rtv<-getAssetName()
      }
    } else { # RPanelTag whenever getAssetName is NULL???
      rtv<-RPanelTag
    }
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
  ctype=NULL
  colName<-getTibColumnName() # i.e. selectedAsset$columnName
  if(!is.null(colName)){
    columnValues<-getTib()[[colName]]
    if(!is.null(columnValues)){
      ctype=extractColType(columnValues)
    }
  }
  return(ctype)
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
    if(!is.null(colType)){
      rtv<-NULL #should never happen!!!
    } else if(identical(colType,'point')){
      rtv<-c('point', 'matrix')[ getSelIndex() ]
    } else {
      rtv<-'value'
    }
  } else {
    rtv<-nameType
  }
  rtv
})

# returns true iff editing tib contents
getTibEditState<-reactive({
  rtv<-getSourceType()==svgPanelTag && 
    !is.null(getPlotState()) && 
    getPlotState() %in%  c("point", "value", "matrix")
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
  if(hasError()){
    rtv<-errorPanelTag
  } else if (panels$sourceType %in% c( rmdPanelTag, textPanelTag, snippetPanelTag, javascriptPanelTag, appPanelTag) ){
    rtv<-panels$sourceType
  } else {
    rtv<-getPlotState()
  }
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
      grepl("class\\s*=\\s*'adjustable'",code ) || 
        grepl('class\\s*=\\s*"adjustable"',code )
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




# 
observeEvent( c( getRightMidPanel(), hasPreProcChoices() ), {
  dropdownId= "preProcDropDown" #paste0("PtPreProc",id)
  div_id=paste0(dropdownId,'Div')
  if( !is.null(getRightMidPanel()) &&
      hasPreProcChoices()
  ){
    removeCssClass( id=div_id, class="hiddenPanel")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  } else {
    addCssClass( id=div_id, class="hiddenPanel")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  }
},
label='PanelCoordinator.R:: c(getRightMidPanel(), hasPreProcChoices())'
)
