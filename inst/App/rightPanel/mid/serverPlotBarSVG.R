

# ===============BEGIN SERVER Module svgSVGMod=======================


statusPlotSVG<-callModule(
  module=modulePlotSVGr,
  id="svgSVGMod",
  svgID='ptR_SVG_SVG',
  showPts.compound=reactive({
    NULL
  }),
  ptrDisplayScript = reactive({ list("") }), 
  getSVGWH,
  getSvgGrid,
  getBackDrop,
  getCode4Rendering,
  getErrorMssg,
  getTibNRow=getTibNRow,
  getDirPath=getDirPath
)

#error handler
observeEvent(c(statusPlotSVG$status(), statusPlotSVG$WH()), {
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){ 
    mssg$err<-paste(mssg$err, status$message, "cannot plot: code02\n", collapse="\n")
  } else {
    wh<-statusPlotPoint$WH()
    getSVGWH(wh)
  }
})

