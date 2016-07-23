
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)
library(shinyAce)
source("utilStyle.R")

version="ptR (v.0.3.1)"

shinyUI(fluidPage(
  singleton(tags$head(
    tags$script(src = "message-handler.js"),
    tags$style("#tagPts{margin: 0px; font-size: 12px;}"),
    tags$style("#tagIndx{margin: 0px; font-size: 12px;}"),
    tags$style("#tagCol{margin: 0px; font-size: 12px;}"),
    tags$style("#tagColVal{margin: 0px; font-size: 12px;}"),
    tags$style("#tagValEd{margin: 0px; font-size: 12px;}"),
    tags$style(HTML(styleSpec))
  )),
  absolutePanel( left=0, top=0, width=650, #editor panel
    navbarPage(version, fluid=TRUE, 
      id="editNavBar",
      navbarMenu("File", 
        tabPanel("New"),
        tabPanel("Open"),
        tabPanel("Save")
      ),
      navbarMenu("Code", 
        tabPanel("Edit svgR" , value="Source")
      ),
      tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >User Guide </a></li>")),
      tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
    ),
    style=cstyle$sidePanel, 
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
  absolutePanel( top=0, left=670, width=650, height=660, style=cstyle$wellPanel, 
    navbarPage(version,  id="plotNavBar", fluid=TRUE, 
    #---------------plotNavBar:points  ------------------------------------
      tabPanel("Points" ), #end of tab panel "Points"
      #---------------plotNavBar:TAGS  ------------------------------------
      navbarMenu("Tags", 
                 tabPanel("Value", value="tagValues"),
                 tabPanel("Drag",  value="dragTag")
                 # tabPanel("Wag"),
                 # tabPanel("Mag")
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
    conditionalPanel( "input.plotNavBar!='Log'",
      absolutePanel( top=130, left=0, right=0,  draggable=FALSE,
                     style=cstyle$svg, htmlOutput("svghtml")
      )
    ),
    br(),
    #-------CONDITIONAL----plotNavBar:POINTS---------------------------------
    uiOutput("PointsPanel"),
    uiOutput("TagDragPanel"),
    uiOutput("TagValuesPanel"),
    
    #-------CONDITIONAL----plotNavBar:BUTTONS---------------------------------
      conditionalPanel( "input.plotNavBar=='Points'",
        absolutePanel( bottom=0, left=0, width=650, draggable=FALSE,
                        style="margin:0px; padding:0px;",
          actionButton("forwardPt", label = "Forward Pt", style=cstyle$button),
          actionButton("backwardPt", label = "Backward Pt", style=cstyle$button),
          actionButton("removePt", label = "Delete Pt", style=cstyle$button),
          actionButton("tagPt", label = "Tag Pt", style=cstyle$button)
        )
      )#, #BUTTONS PANEL END
    #   conditionalPanel( "input.plotNavBar=='Tags'",
    #                   absolutePanel( bottom=0, left=0, width=650, draggable=FALSE,
    #                                  style="margin:0px; padding:0px;",
    #                                  actionButton("copyTag", label = "Copy", style=cstyle$button),
    #                                  actionButton("pasteTag", label = "Paste", style=cstyle$button),
    #                                  actionButton("deleteTag", label = "Delete", style=cstyle$button),
    #                                  actionButton("moveTag", label = "Move", style=cstyle$button)
    #                   )
    # ) #BUTTONS PANEL END
    
    ) # end of absolute panel containing navbar
  ) # end of fluidpage
)
#  ------------------END OF SHINY UI------------------------------------------------------

