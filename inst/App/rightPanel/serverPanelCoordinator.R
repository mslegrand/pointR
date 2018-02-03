

output$TopRightPanel<-renderUI({
  moduleEdTibUI("tagValBar", input, output)
})

output$BottomRightPanel<-renderUI({
  moduleFooterRightUI("footerRight", input, output)
})


output$MidRightPanel<-renderUI({
  chosenRightMidPanel<-getRightMidPanel2()
  if (chosenRightMidPanel=='point'){
    modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightMidPanel=='value'){
    modulePlotSVGrUI("svgTagValsMod")
  } else if (chosenRightMidPanel=='matrix'){
    modulePlotSVGrUI("svgTagDragMod")
  } else if (chosenRightMidPanel == transformTag ){
    modulePlotSVGrUI("svgTransformMod")
  } else if( chosenRightMidPanel == svgPanelTag ){
    modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightMidPanel==logTag){
    moduleLogUI("errLogMod")
  }
})


svgPanelTag<-'svgPanel'

panels<-reactiveValues(
  left='source',   #to be used as editor name later, for connecting to right graphics
  sourceType=svgPanelTag 
  #  sourceType = 'svgPanel'  means svgR code
  #  sourceType = 'logPanel' means plain R code or error
  #  sourceType is set from processCommit
)

setSourceType<-function( sourceType ){
  if(!missing(sourceType)){
    panels$sourceType=sourceType
  }
}

# returns type corresp to name: 'tib', 'logPanel', 'transform'
getNameType<-reactive({
  if(!is.null(getTibName())){
    if (getTibName() %in% names(getPtDefs()$tib)){
      'tib'
    } else {
      getTibName()
    }
  } else {
    logTag
  }
})

#returns the type of column, which can be 'point', 'list', 'numeric', 'colourable', 'value'
getColumnType<-reactive({
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  if(!is.null(columnValues)){
    if( is.list(columnValues) ){
      if(all(sapply(columnValues, function(m){ is.matrix(m) && dim(m)[1]==2}))){
        return('point')
      } else {
        return('list')
      }
    }
    if(is.numeric(columnValues)){
      return('numeric')
    }
    if(is.character(columnValues)){
      if( isColorString(columnValues)){
        cat('column is colourable\n\n')
        return('colourable')
      } else {
        return('character')
      }
    } else {
      return('value')
    }
  }
  return(NULL)
})

# returns state: 'point', 'matrix', 'value', 'logPanel', 'transform'
getPlotState<-reactive({
  nameType<-getNameType()
  if(nameType=='tib'){
    colType<-getColumnType()
    if(colType=='point'){
      c('point', 'matrix')[ getSelIndex() ]
    } else {
      'value'
    }
  } else {
    nameType
  }
})

getRightMidPanel2<-reactive({
  if(panels$sourceType==logTag || is.null(getPlotState() )){
    rtv<-logTag
  } else {
    rtv<-getPlotState()
  }
  rtv
})


getRightPanelChoices<-reactive({ # includes names of tibs
  if(panels$sourceType==logTag){
    choices=logTag
  } else {
    ptDefs<-getPtDefs()
    choices<-names(getPtDefs()$tib)
    if( usingTransformDraggable() ){
      choices<-c(choices, transformTag)
    }
    choices<-c(choices, svgPanelTag, logTag)
  }
  choices
})

getRightPanelName<-reactive({  #used only by editTib
  if(panels$sourceType==logTag){
    return(logTag)
  } else {
    return(selectedTibble$name)
  }
})

is.tibName<-function(x){ !is.null(x) || x==logTag || x==transformTag}

getTibEditState<-reactive({
  (panels$sourceType)==svgPanelTag && !is.null(getPlotState())
    getPlotState() %in%  c("point", "value", "matrix")
    #(panels$state %in% c("point", "value", "matrix"))
})


usingTransformDraggable<-reactive({
  length(getCode()) >0 &&
    nchar(getCode())>0 &&
    ( 
      grepl("class\\s*=\\s*'draggable'",getCode() ) || 
        grepl('class\\s*=\\s*"draggable"',getCode() )
    )
}) 
