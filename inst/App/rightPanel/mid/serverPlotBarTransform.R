#serverBarPlotTransform



# output$svgTransformPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar =='Transforms'", modulePlotSVGrUI("svgTransformMod"))
# })



getCodeTransform<-reactive({
  src<-getCode()
  src<-usingDraggable(src, selectedTibble$transformType)
})


statusPlotTransform<-callModule(
  module=modulePlotSVGr,
  id="svgTransformMod",
  svgID='ptR_SVG_TRANSFORM',
  showPts.compound=NULL,
  ptrDisplayScript = reactive({ 
    type=getTransformType()
    svgToolsScript(type) 
  }),
  #ptrDisplayScript, #
  getSVGWH,
  showGrid,
  getCode,
  getCode2 = getCodeTransform,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",NULL"
)

#error handler
observeEvent(statusPlotTransform$status(), { 
  status<-statusPlotTransform$status()
  if(status$state!="PASS"){
    # cat("statusPlotTransform$status error\n")
    setSourceType(sourceType='logPanel')
    #updateRightPanel('logPanel')
    mssg$err<-status$messages
  }
})

