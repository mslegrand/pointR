
output$TopRightPanel<-renderUI({
  tagList(
      div( "class"='topHeadPanel',
      moduleEdAssetUI("edAssetCh", input, output),
      div( "class"='topHeadTibEdit',
        moduleEdTibUI("tagValBar", input, output)
      ),
      div( "class"='topHeadTibEdit',
        moduleEdTransformUI("edTransform", input, output)
      )
    )
  )
})

output$LeftMidRightPanel<-renderUI({
  moduleRowDNDUI("rowDND", input, output)
})

output$BottomRightPanel<-renderUI({
  moduleFooterRightUI("footerRight", input, output)
})

output$MidRightPanel<-renderUI({
  
  chosenRightMidPanel<-getRightMidPanel()
  cat("chosenRightMidPanel=",format(chosenRightMidPanel),"\n")
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
  } else if( chosenRightMidPanel == errorPanelTag ){
    # cat('about to"errLogMod"\n')
    moduleLogUI("errLogMod")
  }else if (chosenRightMidPanel ==  RPanelTag ){
    # cat('about to"capturedLogMod"\n')
    moduleLogUI("capturedLogMod")
  } else if( chosenRightMidPanel == rmdPanelTag ){
    modulePlotRmdUI("rmdMod")
  }
  
  
  
})
