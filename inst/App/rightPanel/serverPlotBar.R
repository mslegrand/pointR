#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if('character' %in% class(cmd)){
  }
  if(is.null(cmd)){
    cmd<-"tibEditor" 
  }
  if(cmd %in% c( "Points", 'tibEditor', 'tagDrag', 'Transforms', 'log')){
    updateRightPanel(cmd)
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
  if(cmd == 'cmdNewColumn'){
    showModal( addNewColModal() )
  }
  if(cmd == 'cmdDeleteColumn'){
    columnName<-getTibColumnName()
    showModal(deleteColumnModal(columnName))
  }
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
  
})


observeEvent(input$useTribble,{
  if(editOption$useTribbleFormat!=input$useTribble){
    editOption$useTribbleFormat=input$useTribble
    newPtDefs<-getPtDefs()
    sender='useTibble'
    updateAceExtDef(newPtDefs, sender=sender)
  }

})

output$TopRightPanel<-renderUI({
  chosenRightPanel<-rightPanel() 
  # cat('output$TopRightPanel:: chosenRightPanel=',chosenRightPanel,"\n")
  if(chosenRightPanel=="Points"){
    modulePointsBarUI("pointsBar")
  } else if (chosenRightPanel=='tibEditor'){
    moduleEdTibUI("tagValBar", input, output)
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
 
  chosenRightMidPanel<-rightMidPanel()
  #cat('output$MidRightPanel:: chosenRightPanel=',chosenRightMidPanel,"\n")
  if(chosenRightMidPanel=="Points"){
    modulePlotSVGrUI("svgPointsMod")
  } else 
  if (chosenRightMidPanel=='tibEditor.point'){
        modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightMidPanel=='tibEditor.value'){
        modulePlotSVGrUI("svgTagValsMod")
  } else if (chosenRightMidPanel=='tibEditor.matrix'){
        modulePlotSVGrUI("svgTagDragMod")
  } else if (chosenRightMidPanel == "tibEditor.transform" ){
    modulePlotSVGrUI("svgTransformMod")
  } else if (chosenRightMidPanel=="logPanel"){
    moduleLogUI("errLogMod")
  }
})

