
#----------------------------------------------------------------
#------------SVG DISPLAY--------------------------------------------
#----------------------------------------------------------------


showPts.valTag %<c-% function(
  ptName=NULL, 
  pts=NULL, 
  rowIndex=NULL,
  ptDisplayMode ,
  vbScaleFactor=1
  ){
  if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
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
    lapply(offRows, function(i){
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
           list(
             circle(cxy=m[,j], r=8),
             if(ptDisplayMode=="Labeled"){
               text( paste(j), cxy=m[,j]+10*c(1,-1),  stroke='black', font.size=12) 
             } else {
               NULL
             }
           )
         })
        )
      }
    }),
    if(length(mRow)==0){
      NULL
    } else {
          g( opacity=opacity[rowIndex], 
       fill='purple',
       transform="matrix(1 0 0 1 0 0)", 
       onmousedown=onMouseDownTxt,
       tid=paste0("ptR_Tag_",rowIndex),
       lapply(seq(ncol(mRow)), function(j){
         list(
           circle(   cxy=mRow[,j], r=8),
           if(ptDisplayMode=="Labeled"){
             text(paste(j), cxy=mRow[,j]+10*c(1,-1),  stroke='black', font.size=12) #opac)
           } else {
             NULL
           }
         )
       })
      )
    }
  ) #end list
} #end showPts



statusPlotTagVal<-callModule(
  module=modulePlotSVGr,
  id="svgTagValsMod",
  svgID='ptR_SVG_TagVal',
  showPts.compound=reactive({
    function(vbScaleFactor=1){
      showPts.valTag(
        ptName=getAssetName(), 
        pts=getTibPts(), 
        rowIndex=getTibRow(),
        ptDisplayMode=getDisplayMode() ,
        vbScaleFactor
      )
    }
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagVal") }), 
  useKeyMouseScript=TRUE,
  getSVGWH,
  getSvgGrid,
  getBackDrop,
  getCode4Rendering,
  getErrorMssg,
  getTibNRow=getTibNRow,
  getDirPath=getDirPath
)

observeEvent(statusPlotTagVal$status(), {
  status<-statusPlotTagVal$status()
  if(status$state!="PASS"){
   mssg$err<-paste(mssg$err, status$message, "cannot plot: code03\n", collapse="\n")
  }
})




