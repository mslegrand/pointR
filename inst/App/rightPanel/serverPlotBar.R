#serverPlotBar

observeEvent(input$plotNavBar, {
  cmd<-input$plotNavBar$item
  if(is.null(cmd)){
    cmd<-"Points"
  }
  if(!is.null(cmd)){
    panels$right<-cmd
    dirtyDMDM(session, "plotNavBar")
  }
})

output$TopRightPanel<-renderUI({
  tmp<-rightPanel()
  
  if(rightPanel()=="Points"){
    modulePointsBarUI("pointsBar")
  } else if (rightPanel()=='tagValues'){
    moduleTagValUI("tagValBar", input, output)
  } else if (rightPanel()=='tagDrag'){
    moduleTagDragUI("tagDragBar")
  } else if (rightPanel()=='Transforms'){
    
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
  } else if (rightPanel()=="log"){
    absolutePanel(  draggable=FALSE,
                    "class"="cLogText",
                    verbatimTextOutput("out_log"))
  } 
})



output$MidRightPanel<-renderUI({
  if(rightPanel()=="Points"){
    modulePlotSVGrUI("svgPointsMod")
  } else if (rightPanel()=='tagValues'){
    modulePlotSVGrUI("svgTagValsMod")
  } else if (rightPanel()=='tagDrag'){
    modulePlotSVGrUI("svgTagDragMod")
  } else if (rightPanel()=='Transforms'){
    modulePlotSVGrUI("svgTransformMod")
  } else if (rightPanel()=="log"){
    taglist(absolutePanel(  draggable=FALSE,
                            "class"="cLogText",
                            verbatimTextOutput("out_log")))
  } 
})

