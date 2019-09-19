
output$TopRightPanel<-renderUI({
  tagList(
      div( "class"='topHeadPanel',
      moduleEdAssetUI("edAssetCh", input, output),
      div( "class"='topHeadTibEdit',
        moduleEdTibUI("tagValBar", input, output),
        moduleEdTransformUI("edTransform", input, output)
      )
    )
  )
})

# output$LeftMidRightPanel<-renderUI({
#   moduleRowDNDUI("rowDND", input, output)
# })

output$BottomRightPanel<-renderUI({
  tagList(
      moduleFooterRightUI("footerRight", input, output)
  )
})

output$MidRightPanel<-renderUI({
  # cat(">---> output$MidRightPanel\n")
  chosenRightMidPanel<-getRightMidPanel()
  # cat("chosenRightMidPanel=",format(chosenRightMidPanel),"\n")
  if (chosenRightMidPanel=='point'){
    # cat("chosenRightMidPanel=1\n")
    modulePlotSVGrUI("svgPointsMod", input, output)
  } else if (chosenRightMidPanel=='value'){
    # cat("chosenRightMidPanel=2\n")
    modulePlotSVGrUI("svgTagValsMod", input, output)
  } else if (chosenRightMidPanel=='matrix'){
    # cat("chosenRightMidPanel=3\n")
    modulePlotSVGrUI("svgTagDragMod", input, output)
  } else if (chosenRightMidPanel == transformTag ){
    # cat("chosenRightMidPanel=4\n")
    modulePlotSVGrUI("svgTransformMod", input, output)
  } else if( chosenRightMidPanel == svgPanelTag ){
    # cat("chosenRightMidPanel=5\n")
    modulePlotSVGrUI("svgSVGMod", input, output) 
  } else if( chosenRightMidPanel == errorPanelTag ){
     # cat('about to"errLogMod"\n')
    # cat("chosenRightMidPanel=6\n")
    moduleLogUI("errLogMod", input, output)
  } else if (chosenRightMidPanel ==  RPanelTag ){
    # cat('about to"capturedLogMod"\n')
    # cat("chosenRightMidPanel=7\n")
    moduleLogUI("capturedLogMod", input, output)
  } else if (chosenRightMidPanel ==  appPanelTag ){
    if(usingElectron){
      # cat('************ inside output$MidRightPanel: aptRunner***************\n')
      moduleLogUI("aptRunnerLogMod", input, output)
    } else {
      div( img(src="ptR/pointRLogo.SVG") )
    }
  } else if( chosenRightMidPanel == rmdPanelTag ){
    # cat("chosenRightMidPanel=8\n")
    modulePlotRmdUI("rmdMod", input, output)
  } else if(chosenRightMidPanel == textPanelTag){
    # cat("chosenRightMidPanel=9\n")
    div( img(src="ptR/pointRLogo.SVG") ) 
    #Todo add something about sponsors.
  }  else if(chosenRightMidPanel == snippetPanelTag){
    # cat("chosenRightMidPanel=10\n")
    div( img(src="ptR/pointRLogo.SVG") ) 
    #Todo add something about sponsors.
  } else {
    # cat("chosenRightMidPanel=11\n")
    div( img(src="ptR/pointRLogo.SVG"))
  }
  
  
})
