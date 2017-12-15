#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    cmd<-"Points" 
  }
  if(cmd %in% c( "Points", 'tagValues', 'tagDrag', 'Transforms', 'log')){
    panels$right<-cmd #Point, tagValues, tagDrag, transfo
    cat('barName changed\n')
  }
  if(cmd == 'cmdShowGrid'){
    renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    setDisplayOption(showGrid=TRUE)
  }
  if(cmd == 'cmdHideGrid'){
    renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    setDisplayOption(showGrid=FALSE)
  }
  
  if(cmd == 'cmdHidePoints'){
    renameDMDM(session,  "plotNavBar", "cmdHidePoints", "Hide Points", newValue="cmdShowPoints")
    setDisplayOption(ptMode='Hidden')
  }
  if(cmd == 'cmdShowPoints'){
    renameDMDM(session,  "plotNavBar", "cmdShowPoints", "Show Points", newValue="cmdHidePoints")
    setDisplayOption(ptMode='Normal')
  }
  if(cmd == 'cmdHidePointLabels'){
    renameDMDM(session,  "plotNavBar", "cmdHidePointLabels", "Hide  Labels", newValue="cmdShowPointLabels")
    setDisplayOption(ptMode='Normal')
  }
  if(cmd == 'cmdShowPointLabels'){
    renameDMDM(session,  "plotNavBar", "cmdShowPointLabels", "Show  Labels", newValue="cmdHidePointLabels")
    setDisplayOption(ptMode='Labeled')
  }
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
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

