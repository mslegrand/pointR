

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

# -----------Grid ------------- 

svgGrid<-reactiveValues(
  show=FALSE,
  dx=50,
  dy=50,
  color='lightgrey'
)

getSvgGrid<-reactive({
  reactiveValuesToList(svgGrid)
})

setSvgGrid<-function(show,color,dx,dy){
  if(!missing(show)){
    svgGrid$show<-show
  }
  if(!missing(color)){
    svgGrid$color<-color
  }
  if(!missing(dx)){
    svgGrid$dx<-dx
  }
  if(!missing(dy)){
    svgGrid$dy<-dy
  }
}


observeEvent( input$Hspacing, {
  browser()
  dx<-as.numeric(input$Hspacing)
  svgGrid$dx=dx
  
})

observeEvent( input$Vspacing, {
  dy<-as.numeric(input$Vspacing)
  svgGrid$dy=dy
})


#--Backdrop-----------------------------------

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





