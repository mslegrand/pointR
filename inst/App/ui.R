
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


version="ptR (v.0.3.5.3)"


shinyUI(  
  div( id="mySplitter", 
    singleton(
      tags$head(
      tags$link(rel = "stylesheet", type = "text/css", title="customStyle", href = "customStyle.css"),
      tags$link(id='shinyAceStyle', rel = 'stylesheet', type = 'text/css', href = 'shinyAce.css'),
      tags$script(src="splitter.js" ), 
      tags$script(src="customSplit.js" ),
      tags$script(src="api.js"),
      tags$script(src="aceExt.js")
    )),
      #-------------left panel begin--------------------------------------------------
      div( id='leftPanel', class='backPanel', #left panel 
        navbarPage(version, fluid=FALSE, position="static-top", 
          id="editNavBar",
          navbarMenu("File", 
            tabPanel("",value="tab1"), #fake way to mimic button
            tabPanel("New"),
            tabPanel("Open"),
            tabPanel("Save"),
            tabPanel("Save As...", value="saveAs"),
            tabPanel("Export as SVG"),
            tabPanel("Quit", value="quit")
          ),
          navbarMenu("Options", 
            tabPanel("Theme" ),
            tabPanel("Font Size"), 
            tabPanel("Indentation") #,tabPanel("Prepoc (not implemented)" )
          ),
          navbarMenu("Tools", 
                     tabPanel("Preprocessor (Not implemented)" )
          ),
          navbarMenu("Links", 
            tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
            tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
          )
        ), #end  navBarPage (edit)
  
        h3(textOutput( "fileName"), style="white-space:nowrap;"),
        absolutePanel( 
          "class"="cSvgHtml", 
          overflow= "hidden",
          draggable=FALSE,
          shinyAce4Ptr( 
            outputId = "source", value="",  
            mode="ptr", theme=defaultOpts["theme"],
            height = "990px", 
            fontSize=16, autoComplete="live", 
            autoCompleteList =list(svgR=names(svgR:::eleDefs))
          ), 
          inline=FALSE
        ),
        absolutePanel( "class"="footerPanel", 
                       draggable=FALSE,
                actionButton("commit", label = "COMMIT EDIT")
        )
      ), #end left div
      #-------------left panel end--------------------------------------------------
  
      #-------------right panel begin--------------------------------------------------
      div( id='rightPanel', class='backPanel', #right panel
            useShinyjs(),
            extendShinyjs(script="www/menuHelper.js"),
        navbarPage(version,  id="plotNavBar", selected="Points", version, fluid=TRUE, 
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
            absolutePanel(  draggable=FALSE, 
                           "class"="cLogText",
                verbatimTextOutput("out_log")
            )
          )
        ), #navbarPage end
        #--------CONDITIONALS-----------------------------------------------------
        # TODO: need to invoke insertUI/removeUI instead of conditional paneling.
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
  #) # end of fixedPage
)
#  ------------------END OF SHINY UI------------------------------------------------------

