
# controls display of svg rendering
displayOptions<-reactiveValues(
  insertMode=TRUE,
  ptMode="Normal" # can be 'Hidden', 'Normal', 'Labeled'
)

#displayMode<-reactive({displayOptions$ptMode})

# used by newPtLayer
getInsertMode<-reactive({displayOptions$insertMode })

#this is tagDisplay Mode
# nor used
# getDisplayModeTag<-reactive({
#   displayMode() # displayOptions$ptMode
# })

# used by serverPlotBarTagDrag, serverPlotBarPoints, serverPlotBarTagValues
getDisplayMode<-reactive({
  displayOptions$ptMode
})


setDisplayOption<-function( insertMode, ptMode ){
  if(!missing(insertMode)){
    displayOptions$insertMode<-insertMode
  }
  if(!missing(ptMode)){
    displayOptions$ptMode<-ptMode
  }
}






