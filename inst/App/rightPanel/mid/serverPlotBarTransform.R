#serverBarPlotTransform



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
  getTibNRow=getTibNRow 
)

#error handler
observeEvent(statusPlotTransform$status(), { 
  status<-statusPlotTransform$status()
  if(status$state!="PASS"){
    mssg$err<-status$messages
  }
})

