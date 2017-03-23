# http://shiny.rstudio-staging.com/articles/dynamic-ui.html

output$LogPanel<-renderUI({
  conditionalPanel( 
    "input.plotNavBar=='Log'", 
    absolutePanel(  draggable=FALSE,
                    "class"="cLogText",
                    verbatimTextOutput("out_log")
    )              
  )
})

