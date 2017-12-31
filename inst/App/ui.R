
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


version="ptR:v.0.3.5.6.10"

# style="position: fixed; top: -100em" to keep hidden
shinyUI(  
  div( class="pretty-split-pane-frame", id="mySplitter",
    singleton(
      tags$head(
        initResourcePaths(),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "customStyle.css"),
        tags$link(id='shinyAceStyle', rel = 'stylesheet', type = 'text/css', href = 'Acejs/shinyAce.css'),
        tags$script(src="api.js"),
        tags$script(src="splitjs/split-pane.js" ),
        tags$script(src="splitjs/split-pane-ptr.js" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane.css" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane-ptr.css" ),
        tags$link(rel = "stylesheet", type = "text/css", href ="splitjs/split-pane-pretty-frame.css" ),
        tags$link(rel = 'stylesheet', type = 'text/css', href ="customDMDM.css"),
        tags$script(src = 'IOjs/pointsIO.js' ),
        tags$script(src = 'IOjs/tagValIO.js' ),
        tags$script(src = 'IOjs/transIO.js' ),
        tags$script(src = 'IOjs/rotIO.js' ),
        tags$script(src = 'IOjs/scaleIO.js' ),
        tags$script(src = 'IOjs/tagDragIO.js' ),
        tags$script(src = 'ptR/ptRManager.js' )
      )
    ),
    
    div(
      
      class="split-pane vertical-percent",
      useShinyjs(debug = FALSE),
      extendShinyjs(script="www/menuHelper.js"), #appears that only close window is used!
      
      #-------------left panel begin--------------------------------------------------
      #------- left component begin-----------
      div( 
        id='left-component', 
        class="split-pane-component", 
        #-------left bootstrapPage begin ---------
        bootstrapPage(
          #-----left menu begin---------------
          
          dmdMenuBarPage( 
            title=version, 
            # theme=shinytheme("cerulean"),
            menuBarId="editNavBar",
            menuDropdown(
              "File", 
              menuItem("New"),
              menuItem("Open"),
              menuDropdown("Recent Files"),
              menuDivider(),
              menuItem("Save"),
              menuItem("Save As...", value="saveAs"),
              menuItem("Export as SVG"),
              menuDivider(),
              menuItem("Quit", value="quit")
            ),
            menuDropdown(
              'Configure',
              menuDropdown(
                "Editor Options", 
                menuItem("Theme" ),
                menuItem("Font Size"), 
                menuItem("Adjust Tabs",       value="adjustTabs"),
                menuItem("Show White Space"),
                menuItem(defaultOpts$tabType)
              ),
              menuDropdown(
                "Snippets",
                menuItem("Import", value="importSnippetFile"),
                menuItem("Disable")
              )
            ),
            menuDropdown(
              "Tools", 
              menuItem("PointFiltering (Not implemented)" )
            ),
            menuDropdown(
              "Help",
              menuItem("Editor ShortCuts"),
              menuItem("Element Reference"),
              menuDropdown(
                "Useful Links", 
                menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
                menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>")),
                menuItem(HTML("<li><a  href=\"https://www.w3.org/TR/SVG/intro.html\"  target=\"_blank\" >W3C SVG reference</a></li>"))
              ),
              menuItem("About", value="aboutCmd")
            )
          ), 
          #-------left menu end------------
          #-------left content begin--------
          shinyFilesButton("buttonFileOpenHidden", label="", 
                           title="Open File", multiple=FALSE, 
                           class='hiddenButton'),
          shinySaveButton("buttonFileSaveHidden", label="", 
                           title="Save as ...",  list('hidden_mime_type'=c("")) , 
                           class='hiddenButton'),
          shinyFilesButton("buttonSnippetOpen", label="", 
                           title="Import Snippet", multiple=FALSE, 
                           class='hiddenButton'),
          h3(
            textOutput( "fileName"),
            style="white-space:nowrap;"
          ),
          absolutePanel( 
            id='aceContainer',
            "class"="cSvgHtml", 
            style="overflow-y:hidden;",
            overflow= "hidden",
            draggable=FALSE,
            shinyAce4Ptr( 
              outputId = "source",  value="",  
              mode="ptr", theme=defaultOpts["theme"],
              height = "990px", 
              fontSize=16, autoComplete="live", 
              autoCompleteList =list(svgR=names(svgR:::eleDefs))
            ), 
            inline=FALSE
          ),
          absolutePanel( 
            "class"="footerPanel", 
            draggable=FALSE,
            actionButton("commit", label = "COMMIT EDIT") 
          )
          #-------left content end--------
        ) #----end of bootstrapPage
      ), #---end of left-component
      #-------------left panel end--------------------------------------------------
      #-------------divider begin--------------------------------------------------
      div( class="split-pane-divider", id="my-divider"),
      #-------------divider   end--------------------------------------------------
      #-------------right panel begin--------------------------------------------------
      div( #-----right component begin
        id='right-component', 
        class="split-pane-component",  #right panel
        #---right bootstrap page begin--------------
        bootstrapPage(
          dmdMenuBarPage(
            menuBarId="plotNavBar",
            #---------------DisplayOptions----------------------------------------
           
            menuDropdown('Tibble',
              menuItem('New Tibble', value='cmdNewTibble'),
              menuItem('Rename Tibble', value='cmdRenameTibble'),
              menuItem('Delete Tibble', value='cmdDeleteTibble'),
              menuItem(NULL, value='cmdTibbleFormat', checkboxInput('useTribble', 'display as tribble', value = TRUE, width = NULL))
            ),
            menuDropdown('Column',
              menuItem('Add Column', value='cmdNewColumn'),
              menuItem('Delete Column', value='cmdDeleteColumn'),
              menuItem('Rename Column', value='cmdRenameColumn'),
              menuItem('Clone Column', value='cmdCloneColumn'),
              menuItem('Input Using', value='cmdInputUsingColumn')
            ),
            
            menuDropdown('Display', 
                         menuDropdown('Points',
                                      menuItem('Hide Points', value='cmdHidePoints'), 
                                      menuItem('Show Points with Labels', value='cmdShowPointLabels')
                         ),
                         menuDropdown( 'Grid',
                                       menuItem('Show Grid', value='cmdShowGrid'),
                                       menuDropdown('lines',
                                                    menuItem('Width', value='cmdGridLineWidh'), 
                                                    menuItem('Color', value='cmdGridLineColor')
                                       )
                         ),
                         menuDropdown( 'Backdrop',
                                       menuItem('Hide', value='cmdHideBack'),
                                       menuItem('Color', value='cmdBackDropColor')
                         )
            ) ,
            
            #---------------plotNavBar:points  ------------------------------------
            menuItem("Points", value="tibEditor"),
            menuItem("Log", value="logPanel")
          ), 
          #----------end right menu
          #--------begin right content
          uiOutput("TopRightPanel"),
          br(),
          uiOutput("MidRightPanel")
        ) #-----right bootstrap page end----------
      ) #----------right component end---------
      #-------------right panel end--------------------------------------------------
    )
  )
)
  
#  ------------------END OF SHINY UI------------------------------------------------------


