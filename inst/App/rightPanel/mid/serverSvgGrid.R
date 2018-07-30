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
  # browser()
  dx<-as.numeric(input$Hspacing)
  svgGrid$dx=dx
  
})

observeEvent( input$Vspacing, {
  dy<-as.numeric(input$Vspacing)
  svgGrid$dy=dy
})
