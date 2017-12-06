#serverPlotBar

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    cmd<-"Points" 
  }
  panels$right<-cmd #Point, tagValues, tagDrag, transfo
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
  cat('barName changed\n')
})

output$TopRightPanel<-renderUI({
  chosenRightPanel<-rightPanel()
  if(chosenRightPanel=="Points"){
    modulePointsBarUI("pointsBar")
  } else if (chosenRightPanel=='tagValues'){
    moduleTagValUI("tagValBar", input, output)
  } else if (chosenRightPanel=='tagDrag'){
    moduleTagDragUI("tagDragBar")
  } else if (chosenRightPanel=='Transforms'){
    absolutePanel( 
      top=50, left=0, width="100%", 
      "class"="headerPanel", draggable=FALSE,
        tabsetPanel( id="transformOption", 
          tabPanel("Translate"), 
          tabPanel("Rotate"), 
          tabPanel("Scale"),
          
          type="pills"
        ) 
    )
  } else if (chosenRightPanel=="log"){
    absolutePanel(  draggable=FALSE,
                    "class"="cLogText",
                    verbatimTextOutput("out_log"))
  } 
})



output$MidRightPanel<-renderUI({
  chosenRightPanel<-rightPanel()
  if(chosenRightPanel=="Points"){
    modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightPanel=='tagValues'){
    modulePlotSVGrUI("svgTagValsMod")
  } else if (chosenRightPanel=='tagDrag'){
    modulePlotSVGrUI("svgTagDragMod")
  } else if (chosenRightPanel=='Transforms'){
    modulePlotSVGrUI("svgTransformMod")
  } else if (chosenRightPanel=="logPanel"){
    moduleLogUI("errLogMod")
  } 
})

