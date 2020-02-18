library(shiny)
source("shinyInputControl.R")

ui<-fluidPage(
    h1('Test App'),
    h3('current Value'),
    textOutput('currentValue'),
    
    textInput(inputId='updateValue','update value', '200'),
    actionButton('updateButton', label='press to update value'),
    
    shinyInputControl(inputId='myshinyInputControl', wh=c(400,400), value=200 )
)

server<-function(input,output,session){
    output$currentValue<-renderText(input$myshinyInputControl)
    
    
    observeEvent(input$updateButton,{
        value<-input$updateValue
        tryCatch({
        
        # STEP 6.1:
        #   1. prepare value for update:
        #       for non-text values consider using either
        #           i)  value<-eval(parse(text=value)) 
        #       or if value is numeric
        #           ii) value<-as.numeric(value) 
        #   2. add  updateShinyInputControl
     
        }, 
        error=function(e){
            # do nothing , record error
            print('error')
        })
      },
      ignoreInit = TRUE
    )
}

shinyApp(ui=ui, server=server)
