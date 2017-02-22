#serverBarPlotTransform



# output$svgTransformPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar =='Transforms'", modulePlotSVGrUI("svgTransformMod"))
# })



getCodeTransform<-reactive({
  src<-getCode()
  src<-usingDraggable(src)
})

pointSVGList<-callModule(
  module=modulePlotSVGr,
  id="svgTransformMod",
  svgID='ptR_SVG_TRANSFORM',
  showPts.compound=NULL,
  ptrDisplayScript =reactive({ js.scripts[[ input$transformOption ]] }),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 = getCodeTransform,  # (or getCodeTransform)
  getCodeBackup,
  getErrorMssg,
  insert.end=",NULL"
)
