

# ===============BEGIN SERVER Module svgPointsMod=======================

#---------ShowPts----------------------------------

  showPts.PtCmd %<c-% function(
      ptName, 
      pts=NULL,  
      rowIndex=NULL,
      matColIndex=NULL,
      displayOptions=NULL, 
      vbScaleFactor
  ){
    
    
    if(is.null(displayOptions)){
      return(NULL)
    }
   
    displayOpt<-displayOptions
    if(is.null(displayOpt)||is.null(displayOpt$labelMode) || is.null(displayOpt$restrictMode)){ return(NULL)}
    labelColor<-'#000000' # black by default but override if displayOpt specifies it.
    if(!is.null(displayOpt$labelColor)){
      labelColor<-displayOpt$labelColor
    }
    
    # if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    onMouseDownTxt='ptRPlotter_ptR_SVG_Point.selectPoint(evt)'
    if(is.null(pts) ){ return(NULL) } 
    if(length(unlist(pts))<2){ return(NULL)}
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    semitransparent<-0.3
    opacity<-rep(semitransparent, length(pts) )
    opacity[rowIndex]<-1
    
    #form list of  all point renderings
    if(displayOptions$restrictMode==TRUE){
      rows=rowIndex
    } else {
      rows=1:length(pts)
    }
    g(
      lapply(rows, function(i){
      m<-pts[[i]]
      if(length(m)==0){ # or !is(m,'matrix')
        NULL
      } else {
        lapply(seq(ncol(m)), function(j){ #j is the matCol index
          id<-paste("pd",ptName,i,j,sep="-")
          pt<-m[,j]
          color=colorScheme['default']
          
          list(
            if(i==rowIndex && j== matColIndex ){
              circle(class="draggable", 
                     id=id,  
                     cxy=c(0,0), r=9, fill="yellow", 
                     opacity=opacity[i],
                     stroke=colorScheme['selected'], stroke.width=3,
                     transform=list(scale=1/vbScaleFactor,translate=vbScaleFactor*pt),
                     onmousedown=onMouseDownTxt
              )
            } else { #a non-selected point
              circle(class="draggable", 
                     id=id,  
                     cxy=c(0,0), r=8, fill=color, opacity=opacity[i],
                     transform=list(scale=1/vbScaleFactor,translate=vbScaleFactor*pt),
                     onmousedown=onMouseDownTxt
              )
            },
            if(displayOpt$labelMode==TRUE){
              text(paste0(i,",",j), xy=c(10,-10),  
                   stroke=labelColor, font.size=12, opacity=1,
                   transform=list(scale=1/vbScaleFactor,translate=vbScaleFactor*pt)) 
            } else {
              NULL
            }
          )
        }) #end lapply of this row
      }
 
    })#, #end lapply of points
  
    ) #end of g
  } #end showPts.PtCmd





#===============


# !!! todo: recode onmousedown.newPt
newPtLayer %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    onmousedownNewPt="ptRPlotter_ptR_SVG_Point.newPoint(evt)"
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', cursor='crosshair',
         opacity=.0, onmousedown=onmousedownNewPt
    )
  } else {
    NULL
  } 
}
  
#===============
  
statusPlotPoint<-callModule(
  module=modulePlotSVGr,
  id="svgPointsMod",
  svgID='ptR_SVG_Point',
  showPts.compound=reactive({
    function(vbScaleFactor){
      list(
        newPtLayer( getInsertMode(), getSVGWH() ),
        showPts.PtCmd(
          ptName=getAssetName(), 
          pts=getTibPts(), #getPtDefs()$pts[[getPtName()]],
          rowIndex=getTibRow(),
          matColIndex=getTibMatCol(),
          displayOptions=getDisplayOptions(),
          vbScaleFactor=vbScaleFactor
        )
      )
    }
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "Points") }), 
  useKeyMouseScript=FALSE,
  # getSVGWH, #extraneous???
  getSvgGrid,
  getBackDrop,
  getCode4Rendering,
  getEnvList=getEnvList,
  getErrorMssg,
  #getTibNRow=getTibNRow,  #extraneous
  getParMode=getParMode,
  getDirPath=getDirPath
)

#error handler
observeEvent(c(statusPlotPoint$status(), statusPlotPoint$WH()), {
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){ 
    mssg$err<-paste(mssg$err, status$message, "cannot plot: code02\n", collapse="\n")
  } else {
    wh<-statusPlotPoint$WH()
    getSVGWH(wh) #sets the wh value for later use
  }
})




   