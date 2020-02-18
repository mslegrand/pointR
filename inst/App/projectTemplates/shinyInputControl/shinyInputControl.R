library(shiny)
library(svgR)
# library(jsonlite)

try({ removeInputHandler("shinyInputControlBinding") })

# add any helper functions here



# create simple wrapper around svgR code   
shinyInputControlSvgWrapper<-function(params){
  
    params$CMDS<-"alert('Wrapper: new command(s)')"
    
    # STEP 4.2: replace params$CMDS as appropriate
     
    # fill the wrapper
    source('shinyInputControl_svg.R', local=T)$value
}


# Control constructor to insert in app ui
shinyInputControl<-function(inputId, wh=c(400,400), value='whatever' ){
# note: use toJSON for non-trivial initializations  
  # STEP 3.1
  # STEP 3.1.1 Place any preprocessing of (initial input) value(s) here
  # STEP 3.1.2 adjust params list as desired
  params<-list(ID=inputId, WH=wh,  value=value) 
  
  tagList(
      singleton(tags$head(tags$script(src = "shinyInputControl.js"))),
      div( id=inputId, 
           class="shinyInputControl",
           HTML(as.character(
            shinyInputControlSvgWrapper(params)
           )),
           # STEP 3.3 customize for initialization by attaching data-*** to this div
           # Note: 
           #      'data-xxx'=yyy  only accepts vectors of length 1
           #      for more complex data, try using toJSON to convert value
           'data-value'=value
      )        
  )
}


# server to client update
updateShinyInputControl<-function(session, inputId, wh=c(400,400), value='bogus'){
 # Perform any value preprocessing here (toJSON if needed)
 # STEP 6.2  Using Braindead Update Approach:
      # STEP 6.2.1. Recreate svg Tree
      # STEP 6.2.2. Form message
      # STEP 6.2.3. Send message to client
}


# preprocess data returned to server from the client
shiny::registerInputHandler(
  "shinyInputControlBinding", 
  function(value, shinysession, inputId) {
    if(is.null(value) ) {
      return("NULL")
    } else {
      # STEP 5: process value (may use fromJSON)
      
      # STEP 7: add updateShinyInputControl()

      return(value)
    }
  }
)



