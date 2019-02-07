#serverBarPlotTransform



# getCodeTransform<-reactive({
#   src<-getCode()
#   src<-usingDraggable(src, getTransformType())
#   src
# })


statusPlotTransform<-callModule(
  module=modulePlotSVGr,
  id="svgTransformMod",
  svgID='ptR_SVG_TRANSFORM',
  showPts.compound=NULL,
  ptrDisplayScript = reactive({ 
    type=getTransformType()
    svgToolsScript(type) 
  }),
  getSVGWH,
  getSvgGrid,
  getBackDrop,
  getCode = getCode4RenderingTransform,
  getErrorMssg,
  getTibNRow=getTibNRow,
  insert.end=",NULL"
)

#error handler
observeEvent(statusPlotTransform$status(), { 
  status<-statusPlotTransform$status()
  if(status$state!="PASS"){
    mssg$err<-status$messages
  }
})

