
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
    #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    tags$style(HTML(styleSpec))
  )),
  headerPanel(
    h1("pointR  -An svgR programming tool- (v 0.3)", 
       style = "font-family: 'ChunkFiveRegular'; font-style: italic; font-weight: 500; line-height: 1.1;  color: #4d3a7d;")
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
                                        #,
                                        #tabPanel("Insert TagR"),
                                        #tabPanel("Compounds"),
                                        #tabPanel("Options")
                             ),
                             tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >User Guide </a></li>")),
                             tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
                  ),
      style="background-color: #88B0CA; padding-top: 0px;", 

      br(),br(),
      h3(textOutput( "fileName")),
      aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
                 height = "650px", fontSize=16, autoComplete="live", 
                 autoCompleteList =list(svgR=names(svgR:::eleDefs))),
      actionButton("commit", label = "COMMIT EDIT", 
                   style="background-color: #222244; color: white;
                   border-radius: 24px; ")
    ),
#---------------------------------------------------------------
#---------------plotNavBar  ------------------------------------
# svgR plot panel
    mainPanel( width=6, style="background-color: #88B0CA;", 
      navbarPage("Plot Design:",  id="plotNavBar", fluid=TRUE, 
#---------------plotNavBar:points  ------------------------------------
        tabPanel("Points", 
                  wellPanel( 
                         style="background-color: #8888AA; 
                         border-color: #88AAAA; margin-top: 0px; 
                         margin-bottom: 0px;",
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
                             selectInput(
                               "tagFreq", "Auto Tag",
                               multiple=FALSE, size=1, selectize = FALSE,
                               c(list("Off"),1:20), selected="Off", 
                               width="80px"  
                             )
                     ),
                     column(3,
                            checkboxInput("insertMode",
                                          "Insert Mode",
                                          value = TRUE, width = "100px"
                            )
                     )
                  ) 
                )
#             fixedRow( column(6, style = "background-color:yellow;", 
#                                div(style = "height:50px;"))
#             ),
        ),


#---------------plotNavBar:TAGS  ------------------------------------
        tabPanel("Tags",
          wellPanel( 
            style="background-color: #8888AA; border-color: #88AAAA; 
                  margin-top: 0px; margin-bottom: 0px;",
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
                    selectInput(
                      "tagIndx", "Tag Index",
                      multiple=FALSE, size=3, selectize = FALSE,
                      list(), selected=NULL,
                      width="60px"  
                    )
              ),
              column(2, 
                    selectInput(
                      "tagCol", "Column Name",
                      multiple=FALSE, size=3, selectize = FALSE,
                      list(),  selected=NULL, 
                      width="100px"  
                    )
              ),
                     
              column(3, 
                    selectInput(
                      "tagColVal", "Column-Tag Value Choice",
                      multiple=FALSE, size=3, selectize = FALSE,
                      list(),  selected=NULL, 
                      width="100px"  
                    )
              ),
              column(3, textInput("tagValEd", "Alternate Value", 
                                 value=""),
                    actionButton("insertVal2Col", label = "Insert Val", 
                                 style="background-color: #222244; color: white; border-radius: 24px; ")
                    )
              ),
              style="background-color: #8888AA; border-color: #88AAAA; 
                    margin-top: 0px; margin-bottom: 0px; margin-left:0; margin-right:0"
          ) #well panel end
        ), #tab panel Tags end
#---------------plotNavBar:Transform  ------------------------------------

        tabPanel("Transforms", 
               style="background-color: #8888AA; 
                       border-color: #88AAAA; margin-top: 0px; 
                       margin-bottom: 0px;",
               tabsetPanel( id="transformOption",
                            tabPanel("Translate"), 
                            tabPanel("Rotate"), 
                            tabPanel("Scale"),
                            type="pills"
               ) 
        ),

        tabPanel("Log",
         div( style="width:600px ;height: 640px; border: 1px solid darkblue; overflow: auto; background-color: white;",
              verbatimTextOutput("out_log")
         )
        )
      ), #plotNavBar end
#-----------plotNavBar:Plot--------------------------------------------
      conditionalPanel( "input.plotNavBar!='Log'",
        splitLayout(cellWidths = c("70%", "30%"),
                    #h5('svgR plot', style="color: #4d3a7d; margin-top: 0px;margin-bottom: 0px;"),
                    checkboxInput("showGrid", "Show Coordinate Grid", value = TRUE, width = "200px")
        ),
        div( style="width:600px ;height: 540px; border: 1px solid darkblue; overflow: auto; background-color: white;",
             htmlOutput("svghtml")
        )
      )
      ,br(),
#-----------plotNavBar:Buttons--------------------------------------------
      conditionalPanel( "input.plotNavBar=='Points'",
        
        actionButton("forwardPt", label = "Select Forward", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("backwardPt", label = "Select Back", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("removePt", label = "Selected Delete", 
                     style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("tagPt", label = "Selected Tag", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        br(),br()
      )
    )
  )
))

#  ------------------------------------------------------------------------

