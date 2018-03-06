
output$TopRightPanel<-renderUI({
  moduleEdTibUI("tagValBar", input, output)
})

output$LeftMidRightPanel<-renderUI({
  moduleRowDNDUI("rowDND", input, output)
})

output$BottomRightPanel<-renderUI({
  moduleFooterRightUI("footerRight", input, output)
})

output$MidRightPanel<-renderUI({
  
  chosenRightMidPanel<-getRightMidPanel()
  # cat("chosenRightMidPanel=",format(chosenRightMidPanel),"\n")
  if (chosenRightMidPanel=='point'){
    modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightMidPanel=='value'){
    modulePlotSVGrUI("svgTagValsMod")
  } else if (chosenRightMidPanel=='matrix'){
    modulePlotSVGrUI("svgTagDragMod")
  } else if (chosenRightMidPanel == transformTag ){
    modulePlotSVGrUI("svgTransformMod")
  } else if( chosenRightMidPanel == svgPanelTag ){
    modulePlotSVGrUI("svgPointsMod")
  } else if (chosenRightMidPanel %in% c(errorPanelTag, RPanelTag)){
    moduleLogUI("errLogMod")
  }
  
  
  
})
