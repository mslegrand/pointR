

# ===============BEGIN SERVER Module svgSVGMod=======================

#---------ShowPts----------------------------------

showPts.SVGCmd %<c-% function(
  ptName, 
  pts=NULL,  
  rowIndex=NULL,
  matColIndex=NULL,
  ptDisplayMode="Normal"
){
  return (NULL)
} 



newPtLayer %<c-% function(insert, wh=c(1200,800)){NULL}

#===============

statusPlotSVG<-callModule(
  module=modulePlotSVGr,
  id="svgSVGMod",
  svgID='ptR_SVG_SVG',
  showPts.compound=reactive({
    list(
      newPtLayer( false, getSVGWH() ),
      showPts.SVGCmd(
        ptName=getAssetName(), 
        pts=getTibPts(), 
        rowIndex=getTibRow(),
        matColIndex=getTibMatCol(),
        ptDisplayMode=getDisplayMode()
      )
    )
  }),
  ptrDisplayScript = reactive({ list("") }), 
  getSVGWH,
  getSvgGrid,
  getBackDrop,
  getCode4Rendering,
  getErrorMssg,
  getTibNRow=getTibNRow,
  insert.end=",showPts.compound()"
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

