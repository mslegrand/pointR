
# controls display of svg rendering
displayOptions<-reactiveValues(
  insertMode=TRUE,
  ptMode="Normal", # can be 'Hidden', 'Normal', 'Labeled'
  labelMode=FALSE,
  restrictMode=FALSE,
  labelColor='#000000'
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


setDisplayOption<-function( insertMode, ptMode, labelMode, restrictMode ){
  if(!missing(labelMode)){
    displayOptions$labelMode<-labelMode
  }
  if(!missing(restrictMode)){
    displayOptions$restrictMode<-restrictMode
  }
  if(!missing(insertMode)){
    displayOptions$insertMode<-insertMode
  }
  if(!missing(ptMode)){
    displayOptions$ptMode<-ptMode
  }
}
getDisplayOptions<-reactive({
  tmp<-reactiveValuesToList(displayOptions) #how can this possibly work???
})






