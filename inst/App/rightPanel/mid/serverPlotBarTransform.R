#serverBarPlotTransform



statusPlotTransform<-callModule(
  module=modulePlotSVGr,
  id="svgTransformMod",
  svgID='ptR_SVG_TRANSFORM',
  showPts.compound=reactive({NULL}),
  ptrDisplayScript = reactive({ 
    type=getTransformType()
    svgToolsScript(type) 
  }),
  useKeyMouseScript=FALSE,
  # getSVGWH, #extraneous???
  getSvgGrid,
  getBackDrop,
  getCode = getCode4RenderingTransform,
  getEnvList=getEnvList,
  getErrorMssg,
  #getTibNRow=getTibNRow, #extraneous
  getParMode=getParMode,
  getDirPath=getDirPath
)

#error handler
observeEvent(statusPlotTransform$status(), { 
  status<-statusPlotTransform$status()
  if(status$state!="PASS"){
    mssg$err<-status$messages
  }
})

