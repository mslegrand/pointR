
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(svgR)
library(shinyAce)
source("utilStyle.R")

version="ptR (v.0.3.5)"

shinyUI(  
  fixedPage( id="Outer", 
    singleton(
      tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "customStyle.css"),
      #tags$script(src = "message-handler.js"),
      tags$script(src="splitter.js" ), 
      tags$script(src="customSplit.js" )
    )),
    div( id="MySplitter", 
      #-------------left panel begin--------------------------------------------------
      div( class='backPanel', #left panel 
        navbarPage(version, fluid=FALSE, position="static-top", 
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
        ), #end  navBarPage (edit)
  
        h3(textOutput( "fileName"), style="white-space:nowrap;"),
        aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
          height = panelHeights["aceHeight" ], #"490px", 
          fontSize=16, autoComplete="live", 
          autoCompleteList =list(svgR=names(svgR:::eleDefs))
        ),
        actionButton("commit", label = "COMMIT EDIT")
      ), #end left div
      #-------------left panel end--------------------------------------------------
  
      #-------------right panel begin--------------------------------------------------
      div(  class='backPanel', #right panel
            useShinyjs(),
            extendShinyjs(script="www/menuHelper.js"),
        navbarPage(version,  id="plotNavBar", selected="Points", version, fluid=FALSE, 
                 position="static-top", 
          #---------------plotNavBar:points  ------------------------------------
          tabPanel("Points" ), #end of tab panel "Points"
          #---------------plotNavBar:TAGS  ------------------------------------
          navbarMenu("Tags", 
            tabPanel("Value", value="tagValues"),
            tabPanel("Drag",  value="tagDrag")  # tabPanel("Wag"), # tabPanel("Mag")
          ),
          #---------------plotNavBar:Transform  ------------------------------------
          tabPanel("Transforms", 
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
        uiOutput("svgPointsPanel"),
        uiOutput("svgTagValuesPanel"),
        uiOutput("svgTagDragPanel"),
        uiOutput("svgTransformPanel"),
        br(),
        uiOutput("PointsPanel"),
        uiOutput("TagDragPanel"),
        uiOutput("TagValuesPanel")
        #not needed to add uiOutput("TransformPanel"),
      ) # end of right panel
      #-------------right panel end--------------------------------------------------
    ) # splitter end
  ) # end of fixedPage
)
#  ------------------END OF SHINY UI------------------------------------------------------

