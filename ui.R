
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)
library(shinyAce)
source("utilStyle.R")

version="ptR (v.0.3.2)"

shinyUI(fluidPage(
  singleton(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customStyle.css")#,
    #tags$script(src = "message-handler.js"),
    #,tags$script(src = "split.js")
  )),
  absolutePanel( left=0, top=0, width=650,  #editor panel
  #div(id="left", class="split-horizontal",
    navbarPage(version, fluid=TRUE, 
      id="editNavBar",
      navbarMenu("File", 
        tabPanel("New"),
        tabPanel("Open"),
        tabPanel("Save")
      ),
      navbarMenu("Edit", 
        tabPanel("svgR Code " , value="Source"),
        tabPanel("Config (not implemented)" ),
        tabPanel("Prepoc (not implemented)" )
      ),
      tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
      tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
    ),
    #style=cstyle$sidePanel, 
    class='backPanel',
    h3(textOutput( "fileName")),
    aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
      height = panelHeights["aceHeight" ], #"490px", 
      fontSize=16, autoComplete="live", 
      autoCompleteList =list(svgR=names(svgR:::eleDefs))),
    actionButton("commit", label = "COMMIT EDIT", style=cstyle$button)
  ),
#---------------------------------------------------------------
#---------------plotNavBar  ------------------------------------
# svgR plot panel
   absolutePanel( top=0, left=670, width=650, height=660, 
                  class='backPanel',
  # div( id='right', class="split split-horizontal",
    navbarPage(version,  id="plotNavBar", fluid=TRUE, selected="Points",
    #---------------plotNavBar:points  ------------------------------------
      tabPanel("Points" ), #end of tab panel "Points"
      #---------------plotNavBar:TAGS  ------------------------------------
      navbarMenu("Tags", 
        tabPanel("Value", value="tagValues"),
        tabPanel("Drag",  value="tagDrag")  # tabPanel("Wag"), # tabPanel("Mag")
      ),
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
        absolutePanel( top=130, left=0, right=0, draggable=FALSE, style=cstyle$svg,
            verbatimTextOutput("out_log")
        )
      )
    ), #navbarPage end
    #--------CONDITIONALS-----------------------------------------------------
    #-------CONDITIONAL----plotNavBar:SVGHTML---------------------------------
    
    uiOutput("svgPointsPanel"),
    uiOutput("svgTagValuesPanel"),
    uiOutput("svgTagDragPanel"),
    #todo add 
    uiOutput("svgTransformPanel"),

    br(),
    #-------CONDITIONAL----plotNavBar:POINTS---------------------------------
    uiOutput("PointsPanel"),
    uiOutput("TagDragPanel"),
    uiOutput("TagValuesPanel")
    #not needed to add uiOutput("TransformPanel"),
    ) # end of absolute panel containing navbar
  ) # end of fluidpage
)
#  ------------------END OF SHINY UI------------------------------------------------------

