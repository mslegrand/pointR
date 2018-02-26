

# ===============BEGIN SERVER Module svgPointsMod=======================

#---------ShowPts----------------------------------

  showPts.PtCmd %<c-% function(
      ptName, 
      pts=NULL,  
      rowIndex=NULL,
      matColIndex=NULL,
      ptDisplayMode="Normal"
  ){
    # cat("showPts.PtCmd-----------------------------------------------\n")
    # cat("class(ptDisplayMode)="  ,  class(ptDisplayMode),"\n")
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    onMouseDownTxt='ptRPlotter_ptR_SVG_Point.selectPoint(evt)'
    
    # cat('\nshowPts.PtCmd:: class(pts)=',class(pts),'\n')
    # cat('\nshowPts.PtCmd:: length(pts)=',length(pts),'\n')
    if(is.null(pts) ){ return(NULL) } 
    
    
     # cat("pts-----------------------------------------------\n")
     # print(pts)
    if(length(unlist(pts))<2){ return(NULL)}
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    semitransparent<-0.3
    
    # cat("rowIndex",rowIndex,"\n")
    # cat("class(rowIndex)",class(rowIndex),"\n")
    # 
    # cat("matColIndex",matColIndex,"\n")
    # cat("class(matColIndex)",class(matColIndex),"\n")
    
    opacity<-rep(semitransparent, length(pts) )
    opacity[rowIndex]<-1
    
    
    #form list of  all point renderings
    lapply(seq(length(pts)), function(i){
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
                     cxy=pt, r=9, fill="yellow", 
                     opacity=opacity[i],
                     stroke=colorScheme['selected'], stroke.width=3,
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
              )
            } else { #a non-selected point
              circle(class="draggable", 
                     id=id,  
                     cxy=pt, r=8, fill=color, opacity=opacity[i],
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
              )
            },
            if(ptDisplayMode=="Labeled"){
              text(paste0(i,",",j), cxy=pt+10*c(1,-1),  
                   stroke='black', font.size=12, opacity=1) 
            } else {
              NULL
            }
          )
        }) #end lapply of this row
      }
 
    }) #end lapply of points
  } #end showPts.PtCmd





#===============


# !!! todo: recode onmousedown.newPt
newPtLayer %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    onmousedownNewPt="ptRPlotter_ptR_SVG_Point.newPoint(evt)"
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', 
         opacity=.0, onmousedown=onmousedownNewPt)
           #"ptRPlotter_ptR_SVG_Point.newPoint(evt)")
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
    list(
      newPtLayer( getInsertMode(), getSVGWH() ),
      showPts.PtCmd(
        ptName=getTibName(), 
        pts=getTibPts(), #getPtDefs()$pts[[getPtName()]],
        rowIndex=getTibRow(),
        matColIndex=getTibMatCol(),
        ptDisplayMode=getDisplayMode()
      )
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "Points") }), 
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  getTibNRow=getTibNRow,
  insert.end=",showPts.compound()"
)

#error handler
observeEvent(statusPlotPoint$status(), {
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){ 
    # cat("statusPlotPoint$status() error\n")
    setSourceType(sourceType='logPanel')
    #updateRightPanel('logPanel')
    mssg$err<-status$message
  }
})



   