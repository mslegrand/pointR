
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)

library(shinyDMDMenu)
library(shinythemes)

version="ptR (v.0.3.5.6)"


shinyUI(  
  div( class="pretty-split-pane-frame", id="mySplitter",
    singleton(
      tags$head(
        initResourcePaths(),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "customStyle.css"),
        tags$link(id='shinyAceStyle', rel = 'stylesheet', type = 'text/css', href = 'Acejs/shinyAce.css'),
        tags$script(src="api.js"),
        tags$script(src="Acejs/aceExt.js"),
        tags$script(src="splitjs/split-pane.js" ),
        tags$script(src="splitjs/split-pane-ptr.js" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane.css" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane-ptr.css" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane-pretty-frame.css" ),
        tags$link(rel = 'stylesheet', type = 'text/css', href ="customDMDM.css")
      )
    ),
    div(
      class="split-pane vertical-percent",
      useShinyjs(debug = FALSE, showLog = TRUE),
      extendShinyjs(script="www/menuHelper.js"),
      #-------------left panel begin--------------------------------------------------
      div( #left component
        id='left-component', #class='backPanel', 
        class="split-pane-component", #left panel,
        bootstrapPage(
          dmdMenuBarPage( 
            title=version, #fluid=FALSE, position="static-top",
            #theme=shinytheme("cerulean"),
            menuBarId="editNavBar",
            #theme="custom.css",
            menuDropdown("File", 
                         #tabPanel("",value="tab1"), #fake way to mimic button
                         menuItem("New"),
                         menuItem("Open"),
                         menuItem("Save"),
                         menuItem("Save As...", value="saveAs"),
                         menuItem("Export as SVG"),
                         menuItem("Quit", value="quit")
            ),
            menuDropdown('Edit',
              menuDropdown(
                "Options", 
                menuItem("Theme" ),
                menuItem("Font Size"), 
                menuItem("Indentation") #,tabPanel("Prepoc (not implemented)" )
              )
            ),
            menuDropdown(
              "Tools", 
              menuItem("Preprocessor (Not implemented)" )
            ),
            menuDropdown(
              "Links", 
              menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
              menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>"))
            )
          ), #end menu
          # begin content
          h3(textOutput( "fileName"), style="white-space:nowrap;"),
          absolutePanel( 
            id='aceContainer',
            "class"="cSvgHtml", 
            style="overflow-y:hidden;",
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
        ) #end of basicPage
      ), #end of left-component
      #-------------left panel end--------------------------------------------------
      div( class="split-pane-divider", id="my-divider"),
      #-------------right panel begin--------------------------------------------------
      div( #right component
        id='right-component', 
        class="split-pane-component",  #right panel
        bootstrapPage(
          dmdMenuBarPage(
            #version,
            menuBarId="plotNavBar",
            #selected="Points", version, fluid=TRUE, position="static-top",
            #---------------plotNavBar:points  ------------------------------------
            menuItem("Points" ), #end of tab panel "Points"
            #---------------plotNavBar:TAGS  ------------------------------------
            menuDropdown(
              "Tags",
              menuItem("Value", value="tagValues"),
              menuItem("Drag",  value="tagDrag")  # tabPanel("Wag"), # tabPanel("Mag")
            ),
            menuItem("Transforms"),
            menuItem("Log")
          ), #end right menu
          #begin right content
          
          uiOutput("TopRightPanel"),
          br(),
          uiOutput("MidRightPanel")
          
        )
      ) # end right component
      #-------------right panel end--------------------------------------------------
    )
    
    
        
      
  
      #-------------right panel begin--------------------------------------------------
    #div( id='rightPanel', class='backPanel',  #right panel
      #basicPage(
      #       #useShinyjs(), 
      #       #extendShinyjs(script="www/menuHelper.js"),
      ) # end of right panel
      #-------------right panel end--------------------------------------------------
    ) # splitter end
  #) # end of fixedPage
#)
#  ------------------END OF SHINY UI------------------------------------------------------


