

displayOptions<-reactiveValues(
  insertMode=TRUE,
  showGrid=FALSE,
  ptMode="Normal" # can be 'Hidden', 'Normal', 'Labeled'
)
displayMode<-reactive({displayOptions$ptMode})

setDisplayOption<-function( insertMode, showGrid, ptMode){
  if(!missing(insertMode)){
    displayOptions$insertMode<-insertMode
  }
  if(!missing(showGrid)){
    displayOptions$showGrid<-showGrid
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