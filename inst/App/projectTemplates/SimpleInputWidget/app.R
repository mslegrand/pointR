library(shiny)
source("widgetCntrl.R")

ui<-fluidPage(
    widgetCntrl(inputId='widget', wh=c(300,200) ),
    textOutput('result'),
    sliderInput("slider", label='degrees', min=0, max=180, step=1, value=0),
    actionButton('setValueButton',label = 'Set theta')
)

server<-function(input,output,session){
    output$result<-renderText(input$widget)
    observeEvent(input$widget,{
      updateSliderInput(session, "slider", value=input$widget)
    })
    observeEvent(input$setValueButton,{
      if(!identical(input$slider, input$widget)){
        updateWidgetCntrl(session, 'widget', wh=c(300,200), value=input$slider)
      }
    })
}

shinyApp(ui=ui, server=server)
