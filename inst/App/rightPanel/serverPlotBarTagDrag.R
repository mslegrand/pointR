


#-------------------------------------------


  showPts.dragTag %<c-% function(
    ptName=NULL, 
    pts=NULL, 
    rowIndex=NULL,
    ptDisplayMode 
    ){
    cat("\nenter-------------showPts.dragTag-----------------------------------------------\n")
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    
    onMouseDownTxt="ptRPlotter_ptR_SVG_TagDrag.selectElement(evt)" 
    
    cat("showPts.dragTag\n")
    cat("showPts:: rowIndx=", rowIndex, "\n")
    cat("length(ptName)=", length(ptName), "\n")
    cat("length(pts)=", length(pts), "\n")
    if(length(ptName)<1){return(NULL)}
    if(length(pts)<1)  {return(NULL) }
    cat('class(pts)=',class(pts),'\n')
    cat('rowIndex=',rowIndex,'\n')
    cat("length(pts)=",length(pts),"\n")
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
        if(length( mRow)==0){
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


#-------------SVG-------------------------------------
# 
# output$svgTagDragPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar=='tagDrag'", modulePlotSVGrUI("svgTagDragMod"))
# })

statusPlotTagDrag<-callModule(
  module=modulePlotSVGr,
  id="svgTagDragMod",
  svgID='ptR_SVG_TagDrag',
  showPts.compound=reactive({
    showPts.dragTag(
      ptName=getTibName(), 
      pts=getTibPts(), 
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayModeTag() #, 
      #tags=getTagIndexChoices()
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagDrag") }), # reactive({ js.scripts[[ "TagDrag"]] }),
  getSVGWH=getSVGWH,
  showGrid=showGrid,
  getCode= getCode, #srcGet,
  getCode2 =getCode, #srcGet,  # (or getCodeTransform)
  getErrorMssg=getErrorMssg,
  insert.end=",showPts.compound()"
)

observeEvent(statusPlotTagDrag$status(), {
  status<-statusPlotTagDrag$status()
  if(status$state!="PASS"){
    cat("statusPlotTagDrag$status error\n")
    updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    # switch to log 
  }
})


