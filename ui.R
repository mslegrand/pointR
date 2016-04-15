
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)
library(shinyAce)
source("coreConfig.R")
#source("trUtils.R")


shinyUI(fluidPage(
  singleton(tags$head(
    tags$script(src = "message-handler.js"),
    tags$style(HTML(styleSpec))
  )),
  headerPanel(
    h1("pointR  -An svgR programming tool- (v 0.3)", style = cstyle$h1)
    ),
  sidebarLayout( 
    sidebarPanel( width=6, #editor panel
                  navbarPage("Edit:",   fluid=TRUE, 
                             id="editNavBar",
                             navbarMenu("File", 
                                        tabPanel("New"),
                                        tabPanel("Open"),
                                        tabPanel("Save")
                             ),
                             navbarMenu("Code", 
                                        tabPanel("Edit Main" , value="Source")
                             ),
                             tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >User Guide </a></li>")),
                             tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
                  ),
      style=cstyle$sidePanel, 
      br(),br(),
      h3(textOutput( "fileName")),
      aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
                 height = "650px", fontSize=16, autoComplete="live", 
                 autoCompleteList =list(svgR=names(svgR:::eleDefs))),
      actionButton("commit", label = "COMMIT EDIT", 
                   style=cstyle$button)
    ),
#---------------------------------------------------------------
#---------------plotNavBar  ------------------------------------
# svgR plot panel
    mainPanel( width=6, style=cstyle$wellPanel, 
      navbarPage("Plot Design:",  id="plotNavBar", fluid=TRUE, 
#---------------plotNavBar:points  ------------------------------------
        tabPanel("Points", 
          wellPanel( 
            style=cstyle$wellPoint,
            fluidRow(
              column(4, 
                selectInput(
                  "ptSet", "Active Points",
                  multiple=FALSE, size=3, selectize = FALSE,
                  list("x"), selected="x", 
                  width="150px"  
                )
              ),
              column(3,
                radioButtons("ptDisplayMode", 
                  "Display Mode", 
                  c("Normal","Labeled","Hidden"),
                  width = "150px"
                )
              ),
              column(2,
                selectInput("tagFreq", "Auto Tag",
                  multiple=FALSE, size=1, selectize = FALSE,
                  c(list("Off"),1:20), selected="Off", 
                  width="80px"  
                )
              ),
              column(3,
                checkboxInput("insertMode","Insert Mode",
                  value = TRUE, width = "100px"
                )
              )
            ) 
          )#end of well panel
        ), #end of tab panel "Points"
#---------------plotNavBar:TAGS  ------------------------------------
        tabPanel("Tags",
          wellPanel( 
            style=cstyle$wellPoint,
            fluidRow(
              column(2, 
                selectInput(
                  "tagPts", "Tagged Points",
                  multiple=FALSE, size=3, selectize = FALSE,
                  list(),  selected=NULL, 
                  width="100px"  
                )
              ),
              column(2, 
                selectInput("tagIndx", "Tag Index",
                  multiple=FALSE, size=3, selectize = FALSE, list(), selected=NULL,
                  width="60px"  
                )
              ),
              column(2, 
                selectInput("tagCol", "Column Name",
                  multiple=FALSE, size=3, selectize = FALSE, list(),  selected=NULL, 
                  width="100px"  
                )
              ),
                     
              column(3, 
                selectInput("tagColVal", "Column-Tag Value Choice", 
                  multiple=FALSE, size=3, selectize = FALSE,  list(),  selected=NULL, 
                  width="100px"  
                )
              ),
              column(3, 
                textInput("tagValEd", "Alternate Value", value=""),
                actionButton("insertVal2Col", label = "Insert Val", style=cstyle$button)
              )
            ),
            style=cstyle$wellPoint
          ) #well panel end
        ), #tab panel Tags end
#---------------plotNavBar:Transform  ------------------------------------
        tabPanel("Transforms", 
               style=cstyle$wellPoint,
               tabsetPanel( id="transformOption",
                            tabPanel("Translate"), 
                            tabPanel("Rotate"), 
                            tabPanel("Scale"),
                            type="pills"
               ) 
        ),
#---------------plotNavBar:Log  ------------------------------------
        tabPanel("Log",br(),br(),
         div( 
           style=cstyle$log,
             #"width:600px ;height: 640px; border: 1px solid darkblue; overflow: auto; background-color: white;",
              verbatimTextOutput("out_log")
         )
        )
      ), #plotNavBar end
#-----------plotNavBar:Plot--------------------------------------------
      conditionalPanel( "input.plotNavBar!='Log'",
        splitLayout(cellWidths = c("70%", "30%"),
                    checkboxInput("showGrid", "Show Coordinate Grid", value = TRUE, width = "200px")
        ),
        div( style=cstyle$svg,htmlOutput("svghtml"))
      )
      ,br(),
#-----------plotNavBar:Buttons--------------------------------------------
      conditionalPanel( "input.plotNavBar=='Points'",
        
        actionButton("forwardPt", label = "Select Forward", style=cstyle$button),
        actionButton("backwardPt", label = "Select Back", style=cstyle$button),
        actionButton("removePt", label = "Selected Delete", style=cstyle$button),
        actionButton("tagPt", label = "Selected Tag", style=cstyle$button),
        br(),br()
      )
    )
  )
))

#  ------------------------------------------------------------------------

