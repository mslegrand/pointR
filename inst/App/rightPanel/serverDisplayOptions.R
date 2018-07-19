

displayOptions<-reactiveValues(
  insertMode=TRUE,
  ptMode="Normal" # can be 'Hidden', 'Normal', 'Labeled'
)

displayMode<-reactive({displayOptions$ptMode})
getInsertMode<-reactive({displayOptions$insertMode })
#this is tagDisplay Mode
getDisplayModeTag<-reactive({
  displayMode()
})

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






