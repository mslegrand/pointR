library(shiny)
source("shinyInputCntrl.R")

ui<-fluidPage(
    shinyInputCntrl(inputId='myshinyInput', wh=c(400,200), Z=c(2-2i, 2+2i) ),
    h3('current Value'),
    textOutput('currentValue'),
    textInput(inputId='updateValue','update value', ''),
    actionButton('updateButton', label='press to update value')
)

server<-function(input,output,session){
    output$currentValue<-renderText(
        paste('c(', paste(as.character(round(input$myshinyInput, digits=2)), collapse=", "), ')')
    )
   
     observeEvent(input$updateButton,{
         value<-input$updateValue
         print('update button pressed')
         tryCatch({
           
           value<-eval(parse(text=value))
           
           if(length(value)!=2 || class(value)!='complex'){
               stop('invalid input')
           }
           
           updateShinyInputCntrl(session, 'myshinyInput', wh=c(400,200), Z=value)
         }, 
         error=function(e){
            # do nothing , record error
            print('error')
         })
    }
    
    )
}

shinyApp(ui=ui, server=server)
