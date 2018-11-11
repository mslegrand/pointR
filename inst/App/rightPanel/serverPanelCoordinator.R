

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
  cat('>---> getNameType\n')
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
        rtv<-RPanelTag
        #rtv<-getAssetName()
      }
    } else { # RPanelTag whenever getAssetName is NULL???
      rtv<-RPanelTag
    }
    cat('rtv=',format(rtv),"\n")
    cat("<---< getNameType\n")
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
  nameType<-getNameType()
  cat('nameType=',nameType,"\n")
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
  getSourceType()==svgPanelTag && 
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
  cat(">---> getRightPanelChoices\n")
  # browser()
  cat('getSourceType()=', format(getSourceType()),'\n')
  # browser()
  if(hasError() ){ # error: set to  errorPanel
    choices<-errorPanelTag
  } else {
    sourceType<-getSourceType()
    if( identical(sourceType,RPanelTag) ){
      choices=RPanelTag
    } else if( identical(sourceType, rmdPanelTag) ){
      choices=c( rmdPanelTag, RPanelTag )
    } else if( identical(sourceType, snippetPanelTag ) ){
      choices=snippetPanelTag
    } else if( getSourceType()==svgPanelTag){ 
      # browser()
      ptDefs<-getPtDefs()
      # browser()
      choices<-names(getPtDefs()$tib)
      if( usingTransformDraggable() ){
        choices<-c(choices, transformTag)
      }
      choices<-c(choices, svgPanelTag, RPanelTag)
    } else{
      choices<-textPanelTag
    }
  } 
  cat("choices=", paste(choices, collapse=", "),"\n")
  cat("<---< getRightPanelChoices\n")
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


reOrgPanels<-function(id, mode){
  cat(">---> reOrgPanels\n")
  cat('id=',format(id),"\n")
  cat('mode=',format(mode),"\n")
  if(length(id)==0 || length(mode)==0){
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
    hideElement("logo.right")
    hideElement("logo.left")
    # editing ptr
    if(identical(mode,'ptr')){
      showElement("BottomRightPanel")
      showElement("TopRightPanel")
      showElement("snippetToolBarContainer")
      showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      addCssClass( id= 'midRightPanels', class='ctop140')
    } else { # editing other
      removeCssClass( id= 'midRightPanels', class='ctop140')
      hideElement("TopRightPanel")
      hideElement("snippetToolBarContainer")
      hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      if(identical(mode,'ptrrmd')){
        removeCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
      else{
        addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
    }
    showElement("aceToobarTop1")
    showElement("aceToobarTop2")
    showElement("commit")
    showElement("aceTabSet")
    showElement("midRightPanels")
  }
  cat("<---< reOrgPanels\n")
}

# observeEvent(c(getAceEditorId(), getMode()),{
#   id<-getAceEditorId()
#   # if(!identical(selectedAsset$tabId, input$pages)){
#   #   return()
#   # }
#   if(length(id)==0){
#     hideElement("TopRightPanel")
#     hideElement("snippetToolBarContainer")
#     hideElement("aceToobarTop1")
#     hideElement("aceToobarTop2")
#     hideElement("useTribble")
#     hideElement("commit")
#     addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
#     hideElement("aceTabSet")
#     hideElement("midRightPanels")
#     hideElement("BottomRightPanel")
#     showElement("logo.right")
#     showElement("logo.left")
#   } else {
#     hideElement("logo.right")
#     hideElement("logo.left")
#     # editing ptr
#     if(identical(getMode(),'ptr')){
#       showElement("BottomRightPanel")
#       showElement("TopRightPanel")
#       showElement("snippetToolBarContainer")
#       showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
#       addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
#       addClass( id= 'midRightPanels', class='ctop140')
#     } else { # editing other
#       removeClass( id= 'midRightPanels', class='ctop140')
#       hideElement("TopRightPanel")
#       hideElement("snippetToolBarContainer")
#       hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
#       if(identical(getMode(),'ptrrmd')){
#         removeClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
#       }
#       else{
#         addClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
#       }
#     }
#     showElement("aceToobarTop1")
#     showElement("aceToobarTop2")
#     showElement("commit")
#     showElement("aceTabSet")
#     showElement("midRightPanels")
#   }
#    cat('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n')
#   # processCommit()
# },
# label='PanelCoordinator.R:: c(getAceEditorId(), getMode())'
# )
# 


observeEvent( getRightMidPanel(), {
  panel<-getRightMidPanel()
  if( !is.null(panel) && panel %in% c('point','matrix')){
    enableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Point Preprocessor"
    )
  } else {
    disableDMDM(
      session, 
      menuBarId="plotNavBar", 
      entry="Point Preprocessor"
    )    
  }
})



observeEvent( c(getRightMidPanel(), hasPtScript() ), {
  if( !is.null(getRightMidPanel()) && 
      (getRightMidPanel() %in% c('point','matrix')) &&
      hasPtScript() 
  ){
    removeClass( id='PtPreProcDiv', class="hiddenPanel")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    enableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  } else {
    addClass( id='PtPreProcDiv', class="hiddenPanel")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdExportPP")
    disableDMDM( session, menuBarId="plotNavBar", entry="cmdRemovePP")
  }
}, 
label='PanelCoordinator.R:: c(getRightMidPanel(), hasPtScript(), input$pages)' 
)
