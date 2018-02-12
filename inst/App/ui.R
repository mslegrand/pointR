
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


version="ptR:v.0.3.6.0"

# style="position: fixed; top: -100em" to keep hidden
shinyUI(  
  div( class="pretty-split-pane-frame", id="mySplitter",
    singleton(
      tags$head(
        initResourcePaths(),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "customStyle.css"),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "fontello/css/font1_20.css"),
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
          buildLeftMenu(version),
          #-------left menu end------------
          #-------left content begin--------
          shinyFilesButton("buttonFileOpenHidden", label="", title="Open File", multiple=FALSE, 
          shinySaveButton("buttonFileSaveHidden", label="", 
                           title="Save as ...",  list('hidden_mime_type'=c("")) , 
                           class='hiddenButton'),
          shinyFilesButton("buttonSnippetOpen", label="", 
                           title="Import Snippet", multiple=FALSE, 
                           class='hiddenButton'),
          shinySaveButton("buttonExportSVGHidden", label="", 
                          title="Save as ...",  list('hidden_mime_type'=c("")) , 
                          class='hiddenButton'),
          absolutePanel( id='aceTabSet', top=45, left=20, width="100%", 
              tabsetPanel( tabPanel('Unnamed'))
          ),
          absolutePanel( id='aceToobarTop1', 
              top=75, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="30px",
              buildHToolBar(bar1)
          ),
              
          absolutePanel( id='aceToobarTop2', 
              top=105, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="30px",
              buildHToolBar(bar2)
         ),
          absolutePanel( 
            id='aceContainer',
            "class"="cAceContainer", 
            style="overflow-y:hidden;",
            overflow= "hidden",
            draggable=FALSE,
            
            shinyAce4Ptr( 
              outputId = "source",  value="",  
              mode="ptr", theme=defaultOpts["theme"],
              fontSize=16, autoComplete="live", 
              autoCompleteList =list(svgR=names(svgR:::eleDefs))
            ), 
            inline=FALSE
          ),
          absolutePanel( "class"="footerPanel", draggable=FALSE, style="display:inline-block",
             absolutePanel( left=5, bottom=0,
               actionButton("commit", label = "COMMIT EDIT") %>% bs_embed_tooltip(title = "Commit code changes")
             ),
             absolutePanel( left=150, bottom=-10,
               checkboxInput('useTribble', 'display as tribble', value = TRUE, width = NULL)
             )
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
          #----------begin right menu
          buildRightMenu(),
          #----------end right menu
          
          #--------begin right content
          uiOutput("BottomRightPanel"),
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


