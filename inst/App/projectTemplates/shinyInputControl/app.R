library(shiny)
source("shinyInputControl.R")

initialValue='200'
inputId.1<-"shinyInputControlID-1"
  
ui<-fluidPage(
    h1('Test App'),
    h3('current Value'),
    textOutput('currentValue'),
    
    textInput(inputId='updateValue','update value', initialValue),
    actionButton('updateButton', label='press to update value'),
    
    shinyInputControl(inputId=inputId.1,  value= initialValue)
)

server<-function(input,output,session){
    output$currentValue<-renderText(input[[ inputId.1 ]])
    
    
    observeEvent(input$updateButton,{
        value<-input$updateValue
        tryCatch({
        
        # STEP 3.1:
        #   1. prepare value for update:
        #     for example:
        #       for non-text values consider using either
        #           i)  value<-eval(parse(text=value)) 
        #       or if value is numeric
        #           ii) value<-as.numeric(value) 
        #   2. call  updateShinyInputControl
        #      for example:
        #       updateShinyInputControl(session, inputId=inputId.1,  value=value )
     
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
