

displayOptions<-reactiveValues(
  insertMode=TRUE,
  showGrid=FALSE,
  ptMode="Normal" # can be 'Hidden', 'Normal', 'Labeled'
)
displayMode<-reactive({displayOptions$ptMode})

backDrop<-reactiveValues(
  color='white',
  checked=TRUE
)

observeEvent(input$solidBackdrop,{
  backDrop$checked=!input$solidBackdrop
})

observeEvent(input$backdropColour,{
  backDrop$color=input$backdropColour
}, ignoreNULL = TRUE)

getBackDrop<-reactive({
  list(color=backDrop$color, checked=backDrop$checked)
})


setBackDrop<-function(hide, color){
  if(!missing(hide)){
    backDrop$checked<-!hide
  }
  if(color){
    backDrop$color<-color
  }
}


# getGridOpt<-reactive({
#   list(
#     show=displayOptions$showGrid, 
#     color= displayOptions$color,
#     checked=displayOptions$checkGrid
#   )
# })

setDisplayOption<-function( insertMode, showGrid, ptMode, gridColor, gridCheckered){
  if(!missing(insertMode)){
    displayOptions$insertMode<-insertMode
  }
  if(!missing(showGrid)){
    displayOptions$showGrid<-showGrid
  }
  if(!missing(gridColor)){
    displayOptions$colorGrid<-gridColor
  }
  if(!missing(gridCheckered)){
    displayOptions$checkGrid<-gridCheckered
  }
  if(!missing(ptMode)){
    displayOptions$ptMode<-ptMode
  }
  
  
}

#this is tagDisplay Mode
getDisplayModeTag<-reactive({
    displayMode()
})

getDisplayMode<-reactive({
  displayOptions$ptMode
})


getInsertMode<-reactive({displayOptions$insertMode })