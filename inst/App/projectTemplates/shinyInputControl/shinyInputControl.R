library(shiny)
library(svgR)
# library(jsonlite)

try({ removeInputHandler("shinyInputControlBinding") })

# add any helper functions here

shinyInputControl.WH<-c(300,300)

# create simple wrapper around svgR code   
shinyInputControlSvgWrapper<-function(params){
  params$wh<-shinyInputControl.WH
   # STEP 5.2: replace params$CMDS as appropriate
    params$CMDS<-c(
      "alert('Wrapper: new command(s)')" #replace this
    )
    
    # fill the wrapper
    source('shinyInputControl_svg.R', local=T)$value
}


# Control constructor to insert in app ui
shinyInputControl<-function(inputId,  value='whatever' ){
# note: use toJSON for non-trivial initializations  
  # STEP 2.1 Place any preprocessing of (initial input) value(s) here
  
  # STEP 2.2 adjust params list as desired
  params<-list(ID=inputId,   value=value) 
  
  tagList(
      singleton(tags$head(tags$script(src = "shinyInputControl.js"))),
      div( id=inputId, 
           class="shinyInputControl",
           HTML(as.character(
            shinyInputControlSvgWrapper(params)
           )),
           # STEP 2.3 customize for initialization by attaching data-*** to this div
           # Note: 
           #      'data-xxx'=yyy  only accepts vectors of length 1
           #      for more complex data, try using toJSON to convert value
           'data-value'=value #attaches value as string to this div
      )        
  )
}


# server to client update
updateShinyInputControl<-function(session, inputId,  value='bogus'){
 # Perform any value preprocessing here (toJSON if needed)
 # STEP 3.2  Using Braindead Update Approach:
      # STEP 3.2.1. Recreate svg Tree
      # STEP 3.2.2. Form message
      # STEP 3.2.3. Send message to client
}


# preprocess data returned to server from the client
shiny::registerInputHandler(
  "shinyInputControlBinding", 
  function(value, shinysession, inputId) {
    if(is.null(value) ) {
      return("NULL")
    } else {
      # STEP 6.1: process value (may use fromJSON)
      
      # STEP 7: add updateShinyInputControl()

      return(value)
    }
  }
)



