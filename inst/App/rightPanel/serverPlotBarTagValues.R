
#----------------------------------------------------------------
#------------SVG DISPLAY--------------------------------------------
#----------------------------------------------------------------


showPts.valTag %<c-% function(
  ptName=NULL, 
  pts=NULL, 
  rowIndex=NULL,
  ptDisplayMode #,  
  #tags=NULL
  ){
  if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
  
  # cat("rowIndx=", rowIndex, "\n")
  # cat("length(ptName)=", length(ptName), "\n")
  # cat("length(pts)=", length(pts), "\n")
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
        cat('m is null\n')
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
      cat('length(mRow)=0\n')
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
    showPts.valTag(
      ptName=getTibName(), 
      pts=getTibPts(), 
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayMode() #, 
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagVal") }), #ptrDisplayScript = reactive({ js.scripts[[ "TagVal"]]}),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",showPts.compound()"
)

observeEvent(statusPlotTagVal$status(), {
  status<-statusPlotTagVal$status()
  if(status$state!="PASS"){
    # cat("statusPlotTagVal$status() ERRUR\n")
    setPanelValues(sourceType='logPanel')
    #updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    #mssg$err<-'TagValGraphErr'
    # switch to log 
  }
})

# observeEvent(statusPlotTagVal$status(), {
#   status<-statusPlotTagVal$status()
#   if(status$state!="PASS"){
#     updateRightPanel('logPanel')
#     mssg$err<-status$message    # send mssg to log
#     # switch to log 
#   }
# })
# 




