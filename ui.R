
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)
library(shinyAce)
#source("ptDef.R")
source("trUtils.R")


shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML(" 
      @font-face {
               font-family: 'ChunkFiveRegular';
               src: url('Chunkfive-webfont.eot');
               src: url('Chunkfive-webfont.eot?#iefix') format('embedded-opentype'),
               url('Chunkfive-webfont.woff') format('woff'),
               url('Chunkfive-webfont.ttf') format('truetype'),
               url('Chunkfive-webfont.svg#ChunkFiveRegular') format('svg');
               font-weight: normal;
               font-style: normal;
      }
      background-color: #66999;
      h1 {
        font-family: 'ChunkFiveRegular', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }
    "))
  ),
  headerPanel(
    h1("pointR  -An svgR programming tool-", 
       style = "font-family: 'ChunkFiveRegular'; font-style: italic; font-weight: 500; line-height: 1.1;  color: #4d3a7d;")),

  
  sidebarLayout( 
    sidebarPanel( width=6, #editor panel
                  navbarPage("File:", inverse=TRUE,  fluid=FALSE,
                             id="fileNavBar", 
                             tabPanel("newSource"),
                             tabPanel("open"),
                             tabPanel("save"),
                             tabPanel("edit"),
                             tabPanel(HTML("</a></li><li><a target=\"_blank\" href=\"http://mslegrand.github.io/svgR/User_Guide.html\">Users Guide"))
                  ),
      style="background-color: #88AAAA; padding-top: 0px", 

      br(),br(),
      h3(textOutput( "fileName")),
      aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
                 height = "600px", fontSize=16, autoComplete="live", 
                 autoCompleteList =names(svgR:::eleDefs)),
      actionButton("commit", label = "COMMIT EDIT", style="background-color: black; color: white")
    ),

    # svgR plot panel
    mainPanel( width=6, style="background-color: #88AAAA;", 
      navbarPage("Editing:", inverse=TRUE, id="svgNavBar", fluid=FALSE, style="color: #4d3a7d; margin-bottom: 0px;",
          tabPanel("points", style="background-color: #88AAAA; margin-bottom: 0px; margin-top: 0px;",
             selectInput("ptSet", "Selected Pt Vec Def",  list("x"), width="250px"  )
          ),
          tabPanel("translate", style="background-color: #88AAAA; border-color: #88AAAA; margin-top: 0px; margin-bottom: 0px;",
                   #checkboxInput("rotate", "rotate", value = FALSE, width = "250px")
                   #selectInput("transforms", "Selected Transform",  list("translate", "rotate"), width="250px"  )
                   br(),br()
          ),
          tabPanel("rotate", style="background-color: #88AAAA; border-color: #88AAAA; margin-top: 0px; margin-bottom: 0px;",
                    #checkboxInput("rotate", "rotate", value = FALSE, width = "250px")
                    #selectInput("transforms", "Selected Transform",  list("translate", "rotate"), width="250px"  )
                    br(),br()
          )
      ),
      splitLayout(cellWidths = c("70%", "30%"),
                  h3('svgR plot', style="color: #4d3a7d; margin-top: 0px;"),
                  checkboxInput("showGrid", "Show Coordinate Grid", value = TRUE, width = "200px")
      ),
      div( style="width:600px ;height: 560px; border: 1px solid darkblue; overflow: auto;",
           htmlOutput("svghtml")
           )
      ,br(),
      conditionalPanel( "input.svgNavBar=='points'",
        actionButton("removePt", label = "Remove Selected", style="background-color: black; color: white"),
        actionButton("forwardPt", label = "Select Forward", style="background-color: black; color: white"),
        actionButton("backwardPt", label = "Select Back", style="background-color: black; color: white"),
        br(),br()
      )
    )
  )
))

#  ------------------------------------------------------------------------

