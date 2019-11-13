


#-------------------------------------------


  showPts.dragTag %<c-% function(
    ptName=NULL, 
    pts=NULL, 
    rowIndex=NULL,
    ptDisplayMode,
    vbScaleFactor=1
    ){
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    
    onMouseDownTxt="ptRPlotter_ptR_SVG_TagDrag.selectElement(evt)" 
    
    if(length(ptName)<1){return(NULL)}
    if(length(pts)<1)  {return(NULL) }
    if(length(rowIndex)<1 || rowIndex==0){return(NULL)}
    
    semitransparent<-0.3
    colorScheme<-c(default="purple", ending="red", selected="blue")
    color<-colorScheme[1]
 
    opacity<-rep(semitransparent, length(pts)) 
    opacity[rowIndex]<-1 
    rowNums<-seq(length(pts))
    ids<-paste("pd",ptName,rowNums,sep="-")
    offRows<-rowNums[-rowIndex]
    mRow<-pts[[rowIndex]]
      
    list( 
        lapply(offRows, function(i){ #non-selected rows
          m<-pts[[i]]
          if(length(m)==0){
            NULL
          } else {
             g( opacity=opacity[i], 
             fill='purple',
             transform="matrix(1 0 0 1 0 0)", 
             onmousedown=onMouseDownTxt,
             tid=paste0("ptR_Tag_",i),
             lapply(seq(ncol(m)), function(j){ 
               pt=m[,j]
               g(
                  circle(cxy=c(0,0), r=8),
                  if(ptDisplayMode=="Labeled"){
                    text( paste(i), xy=c(10,-10),  stroke='black', font.size=12)
                  } else {
                    NULL
                  },
                  transform=list(scale=1/vbScaleFactor,translate=vbScaleFactor*pt)
               )
             })
            )
          }
        }),
        if(length( mRow)==0){
          NULL
        } else { #selected row=rowIndex
          g( opacity=opacity[rowIndex], 
           fill='purple',
           transform="matrix(1 0 0 1 0 0)", 
           onmousedown=onMouseDownTxt,
           tid=paste0("ptR_Tag_",rowIndex),
           lapply(seq(ncol(mRow)), function(j){
             pt=mRow[,j]
            g(
                circle(cxy=c(0,0), r=8),
                if(ptDisplayMode=="Labeled"){
                    text( paste(rowIndex), xy=c(10,-10),  stroke='black', font.size=12)
                } else {
                    NULL
                },
                transform=list(scale=1/vbScaleFactor,translate=vbScaleFactor*pt)
            )
           })
          )
        }
      ) #end list
  } #end showPts


#-------------SVG-------------------------------------

statusPlotTagDrag<-callModule(
  module=modulePlotSVGr,
  id="svgTagDragMod",
  svgID='ptR_SVG_TagDrag',
  showPts.compound=reactive({
    function(vbScaleFactor){
      showPts.dragTag(
      ptName=getAssetName(), 
      pts=getTibPts(), 
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayMode(),
      vbScaleFactor
      )
    }
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagDrag") }), 
  useKeyMouseScript=TRUE,
  getSVGWH=getSVGWH,
  getSvgGrid=getSvgGrid,
  getBackDrop=getBackDrop,
  getCode= getCode4Rendering, 
  getErrorMssg=getErrorMssg,
  getTibNRow=getTibNRow,
  getDirPath=getDirPath
)

observeEvent(c(statusPlotTagDrag$status(),   statusPlotPoint$WH()), {
  status<-statusPlotTagDrag$status()
  if(status$state!="PASS"){
    mssg$err<-paste(mssg$err, status$message, "cannot plot: code01\n", collapse="\n")
    # switch to log 
  } else {
    wh<-statusPlotPoint$WH()
    getSVGWH(wh)
  }
})


