
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(svgR)
library(shinyAce)
source("trUtils.R")


shinyUI(fluidPage(
  singleton(tags$head(
    tags$script(src = "message-handler.js"),
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
      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-brand:focus {
          color: #5E5E5E;
      }
      .navbar-default .navbar-brand {
          color: #333388;
      }
.navbar .nav > li > a {
    color: #333388;
}
.navbar .nav > li > a:hover {
    float: none;
    color: #FFFFFF;
    background-color: transparent;
}
.navbar-default .navbar-nav > li > a:focus {
  color: white;
  background-color: transparent;
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  color: black;
  font-weight: bold;
  background-color: white;
  opacity: 0.5;
}
      .navbar {
        color: #333388;
        border-color: black;
        font-weight: bold;
        /* Permalink - use to edit and share this gradient: http://colorzilla.com/gradient-editor/#6199c7+-1,cedbe9+0,aac5de+0,aac5de+0,6199c7+0,419ad6+16,419ad6+17,8dceef+51,3a8bc2+84,26558b+100 */
background: #6199c7; /* Old browsers */
background: -moz-linear-gradient(top, #6199c7 -1%, #cedbe9 0%, #aac5de 0%, #aac5de 0%, #6199c7 0%, #419ad6 16%, #419ad6 17%, #8dceef 51%, #3a8bc2 84%, #26558b 100%); /* FF3.6-15 */
background: -webkit-linear-gradient(top, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* Chrome10-25,Safari5.1-6 */
background: linear-gradient(to bottom, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* W3C, IE10+, FF16+, Chrome26+, Opera12+, Safari7+ */
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#6199c7', endColorstr='#26558b',GradientType=0 ); /* IE6-9 */      }
    "))
  )),
  headerPanel(
    h1("pointR  -An svgR programming tool- (v 0.2.4.1)", 
       style = "font-family: 'ChunkFiveRegular'; font-style: italic; font-weight: 500; line-height: 1.1;  color: #4d3a7d;")
    ),
  sidebarLayout( 
    sidebarPanel( width=6, #editor panel
                  navbarPage("Edit:", inverse=F,  fluid=TRUE, 
                             id="fileNavBar",
                             navbarMenu("File", 
                                        tabPanel("New"),
                                        tabPanel("Open"),
                                        tabPanel("Save")
                             ),
                             navbarMenu("View", 
                                        tabPanel("Source"),
                                        tabPanel("Compounds")
                             ),
                             tabPanel(HTML("</a></li><li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >User Guide </li></a>")),
                             tabPanel(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a>"))
                             
                  ),
      style="background-color: #88B0CA; padding-top: 0px;", 

      br(),br(),
      h3(textOutput( "fileName")),
      aceEditor( outputId = "source", value="", mode="r", theme="katzenmilch",
                 height = "600px", fontSize=16, autoComplete="live", 
                 autoCompleteList =names(svgR:::eleDefs)),
      actionButton("commit", label = "COMMIT EDIT", 
                   style="background-color: #222244; color: white;
                   border-radius: 24px; ")
    ),

    # svgR plot panel
    mainPanel( width=6, style="background-color: #88B0CA;", 
      navbarPage("Plot:", inverse=TRUE, id="svgNavBar", fluid=TRUE, 
          tabPanel("Points", 
                    wellPanel( 
                           style="background-color: #8888AA; 
                           border-color: #88AAAA; margin-top: 0px; 
                           margin-bottom: 0px;",
                      fluidRow(
                        column(6, 
                          selectInput(
                            "ptSet", "Selected Pt Vec Def",
                            list("x"), width="150px"  
                          )
                        ),
                        column(6,
                         checkboxInput("insertMode", 
                            "Insert Mode", 
                            value = TRUE, width = "150px"
                          ),
                          checkboxInput("snap2grid", 
                            "Snap to Grid", 
                            value = TRUE, width = "150px"
                          )
                        )
                      )
                    ) 
                  ),
              #      style="background-color: #8888AA; 
              #      border-color: #88AAAA; margin-top: 0px; 
              #      margin-bottom: 0px;",
              # splitLayout(cellWidths = c("50%", "50%"),
              #             selectInput("ptSet", "Selected Pt Vec Def",  
              #                         list("x"), width="200px"  ),
              #             
              #             tabsetPanel( id="pointOption",
              #               tabPanel("Insert"), 
              #               tabPanel("Edit"), 
              #               #tabPanel("Tag"),
              #               type="pills"
              #             )              
              #)

#               fixedRow( column(6, style = "background-color:yellow;", 
#                                div(style = "height:50px;"))
#               ),
                              
             
 #         ),
          tabPanel("Transform", 
                   style="background-color: #8888AA; 
                   border-color: #88AAAA; margin-top: 0px; 
                   margin-bottom: 0px;",
                   tabsetPanel( id="transformOption",
                     tabPanel("Translate"), 
                     tabPanel("Rotate"), 
                     tabPanel("Scale"),
                     type="pills"
                   ) 
          )
      ),
      splitLayout(cellWidths = c("70%", "30%"),
                  h3('svgR plot', style="color: #4d3a7d; margin-top: 0px;"),
                  checkboxInput("showGrid", "Show Coordinate Grid", value = TRUE, width = "200px")
      ),
      div( style="width:600px ;height: 560px; border: 1px solid darkblue; overflow: auto; background-color: white;",
           htmlOutput("svghtml")
           )
      ,br(),
      conditionalPanel( "input.svgNavBar=='Points'",
        actionButton("removePt", label = "Remove Selected", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("forwardPt", label = "Select Forward", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("backwardPt", label = "Select Back", 
                  style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        actionButton("tagPt", label = "Tag", 
                    style="background-color: #222244; color: white;
                  border-radius: 24px; "),
        br(),br()
      )
    )
  )
))

#  ------------------------------------------------------------------------

