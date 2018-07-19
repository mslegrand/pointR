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
