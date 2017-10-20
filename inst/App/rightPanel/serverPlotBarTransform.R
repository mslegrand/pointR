#serverBarPlotTransform



# output$svgTransformPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar =='Transforms'", modulePlotSVGrUI("svgTransformMod"))
# })



getCodeTransform<-reactive({
  src<-getCode()
  cat("input$transformOption=",input$transformOption,"\n")
  src<-usingDraggable(src, input$transformOption)
})


statusPlotTransform<-callModule(
  module=modulePlotSVGr,
  id="svgTransformMod",
  svgID='ptR_SVG_TRANSFORM',
  showPts.compound=NULL,
  ptrDisplayScript =reactive({ js.scripts[[ input$transformOption ]] }),
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
    updateRightPanel('logPanel')
    mssg$err<-status$messages
  }
})

