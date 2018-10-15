
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)




# style="position: fixed; top: -100em" to keep hidden
shinyUI(  
  div( class="pretty-split-pane-frame", id="mySplitter",
    singleton(
      tags$head(
        initResourcePaths(),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "customStyle.css"),
        tags$link(rel = "stylesheet", type = "text/css", id="customStyle", href = "scrollTabs.css"),
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
        tags$script(src='shared/jqueryui/jquery-ui.min.js'),
        tags$script(src = 'ptR/scrollTabs.js' ),
        tags$script(src = 'ptR/snippetScroll.js' ),
        tags$script(src = 'ptR/rowScroll.js' ),
        tags$script(src = 'ptR/ptRManager.js' )
        
      )
    ),
    
    div(
      class="split-pane vertical-percent",
      useShinyjs(),
      extendShinyjs(script="www/menuHelper.js"), #appears that only close window is used!
      
      #-------------left panel begin--------------------------------------------------
      #------- left component begin-----------
      div( 
        id='left-component', 
        class="split-pane-component", 
        #-------left bootstrapPage begin ---------
        bootstrapPage( 
          genShinyOpenFilesButtons(),
          genShinySaveFilesButtons(),
          #-----left menu begin---------------
          buildLeftMenu(version),
          #-------left menu end------------
          #-------left content begin--------

          div( id='aceTabSet', class="container",
               tabsetPanel(id='pages')
          ),
          absolutePanel( id='aceToobarTop1',
              top=75, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="30px",
              buildHToolBar(bar1)
          ),
          absolutePanel( id='aceToobarTop2',
              top=105, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="30px",
              buildHToolBar(bar2)
           ),
          absolutePanel(id="logo.left", top=145, left=0, width="100%", img(src="ptR/pointRLogo.SVG") ),
          div( id='snippetToolBarContainer', "class"="cSnippetToolBarContainer", #draggable=FALSE ,
                tags$ul( id='dndSnippetList', "class"="cSnippetToolBarList",
                  NULL
                ),
                div( id='snippetScrollUp', class='snippetButton  cTop center',
                     span('class'="glyphicon glyphicon-chevron-up")
                ),
                div( id='snippetScrollDown', class='snippetButton cBottom center',
                     span('class'="glyphicon glyphicon-chevron-down")
                )
          ),
          absolutePanel( class="footerPanel", draggable=FALSE, style="display:inline-block",
             absolutePanel( left=5, bottom=0,
               actionButton("commit", label = "COMMIT EDIT") %>% bs_embed_tooltip(title = "Commit code changes")
             ),
             absolutePanel( id='rmdBrowserButtonPanel', left=120, bottom=0,  class='hiddenPanel',
                actionButton("writeNOpen", label = "Open in Browser") %>% bs_embed_tooltip(title = "Save as Html then Open in Browser")
             ),
             absolutePanel( left=150, bottom=-10,
               awesomeRadio('useTribble', NULL, choices=c('Tribble','Tibble'),
                                    selected = "Tribble", 
                                    inline = TRUE, status='success')
             ),
             absolutePanel( right=25, bottom=0, id='selectedDnippetButtonBoxContainer',
                 dropdown( inputId='drippetSelectionDropdown',
                   div( class='backPanel',
                        h5('Dnippet Selection', style='color:white;'),
                       awesomeCheckboxGroup(
                         inputId = "selectedDDDnippets",
                         label = NULL, 
                         choices = c(),
                         selected = c()
                       )
                    ),
                       #style = "unite", 
                       icon=icon("option-vertical", lib = "glyphicon"),
                       #icon=icon("wrench", lib = "glyphicon"),
                       #icon = icon("gear"),
                       status = "primary", width = "300px", size='sm',
                       up=TRUE, right=TRUE,
                       animate = animateOptions(
                         enter = animations$fading_entrances$fadeInLeftBig,
                         exit = animations$fading_exits$fadeOutRightBig
                       )
                 )
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
          buildRightMenu(),
          absolutePanel(id="logo.left", top=145, left=0, width="100%", img(src="ptR/pointRLogo.SVG") ),
          div(newPointPreprocessor(id='BadWolf', title='Point Preprocessor')),
          uiOutput("BottomRightPanel"),
          uiOutput("TopRightPanel"),
          div( id="midRightPanels", class="cMidPanel ctop140",
            div( id='svgOutPanel',  class="cSvgOut", uiOutput("MidRightPanel")),
            div( id='rowOutPanel',  class='cRowOut', 
                 uiOutput("LeftMidRightPanel"),visibility='hidden',
                 div( id='rowScrollUp', class='snippetButton  cTop center cRowButtonUp', height="22px",
                      span('class'="glyphicon glyphicon-chevron-up") 
                 ),
                 div( id='rowScrollDown', class='snippetButton cBottom center cRowButtonDown', height="22px",
                      span('class'="glyphicon glyphicon-chevron-down") 
                 )
              )
          )
        ) #-----right bootstrap page end----------
      ) #----------right component end---------
      #-------------right panel end--------------------------------------------------
    )
  )
)
  
#  ------------------END OF SHINY UI------------------------------------------------------


