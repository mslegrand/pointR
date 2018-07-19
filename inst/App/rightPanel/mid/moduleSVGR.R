
svgToolsScript<-function(type){
  scripts<-list(
    Points=    'var ptRPlotter_ptR_SVG_Point = new PtRPanelPoints("ptR_SVG_Point");',
    TagVal=    'var ptRPlotter_ptR_SVG_TagVal = new PtRPanelTagVal("ptR_SVG_TagVal");',
    Translate= 'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");',
    Rotate=    'var ptRPlotter_ptR_SVG_TRANSFORM_ROTATE = new PtRPanelRotate("ptR_SVG_TRANSFORM");',
    Scale=     'var ptRPlotter_ptR_SVG_TRANSFORM_SCALE = new PtRPanelScale("ptR_SVG_TRANSFORM");',
    TagDrag=    'var ptRPlotter_ptR_SVG_TagDrag = new PtRPanelTagDrag("ptR_SVG_TagDrag");'
  )
  scripts[type]
}

  modulePlotSVGrUI <- function(id, input, output) { 
    ns <- NS(id)
    htmlOutput(ns( "svghtml" ), inline=FALSE)
  }
  
  modulePlotSVGr<-function(
    input, output, session,
    svgID, #='ptR_SVG_Point'
    showPts.compound, # =showsvgRPoints.pts2
    ptrDisplayScript, # =js.scripts[[ "Points"]]
    getSVGWH, 
    getSvgGrid,
    getBackDrop,
    getCode,
    getErrorMssg, 
    getTibNRow,
    insert.end #='showPts.compound()'
  ){
  ns <- session$ns
  user<-  reactiveValues( code="")
  
  rtv<-  reactiveValues(
    status=list(
      state="PASS", 
      message=""
    ),
    WH=NULL
  )
 
  
  # Todo: add the mouseMssg handler
  
  output$svghtml <- renderUI({
    WH<-getSVGWH()
    codeTxt<-getCode()
    if(is.null(getSvgGrid())){return(NULL)}
    
    # why can't I force this???
    showPts.compound=showPts.compound #should be able to force this
    svgid<-paste0('id="', svgID, '",')
    ptrDisplyScriptTxt<-unlist(ptrDisplayScript())

    # transform: modifies src, but omits insert.end
    res<-""
    if(!is.null(codeTxt)){
      tryCatch({
          parsedCode<-parse(text=codeTxt)
          svg<-eval(parsedCode)
          w<-svg$root$getAttr('width')
          h<-svg$root$getAttr('height')
          # browser()
          rtv$WH<-c(w,h)
          svg$root$setAttr('id',svgID)
          if(getSvgGrid()$show==TRUE){ 
            dxy<-c( getSvgGrid()$dx, getSvgGrid()$dy)
            svg$root$prependNode(svgR:::graphPaper( wh=c(w,h), dxy=dxy, labels=TRUE )) 
          }
          if(getBackDrop()$checked==FALSE){
              svg$root$prependChildren(
                svgR:::use(filter=svgR:::filter(filterUnits="userSpaceOnUse", svgR:::feFlood(flood.color=getBackDrop()$color))))
          } else {
             svg$root$prependChildren(
               svgR:::rect(xy=c(0,0), wh=c(w,h),
                  fill=svgR:::pattern( xy=c(0,0), wh=c(20,20), patternUnits="userSpaceOnUse",
                    svgR:::g(
                      svgR:::rect(xy=c(0,0), wh=c(10,10), fill=getBackDrop()$color),
                      svgR:::rect(xy=c(10,10), wh=c(10,10), fill=getBackDrop()$color)
                    )
                  )
                )
              )
          }
          svg$root$prependNode(svgR:::script(ptrDisplyScriptTxt))
          svg$root$prependNode( svgR:::style(".draggable {','cursor: move;','}"))
          if(!is.null(showPts.compound)){
              temp<-svgR(showPts.compound())$root$xmlChildren()
              svg$root$appendChildren(temp)
          }
         as.character(svg)->svgOut 
          res<-HTML(svgOut)
          rtv$status<-list(
            state="PASS",
            message=""
          )
        },
        error=function(e){
          rtv$status<-list(
            state="FAIL", 
            message=paste(getErrorMssg(), e, collapse="\n", sep="\n")
          )
        } 
      )
    }
    
    res
   }) #end of renderUI
  
  list(
    status=reactive({rtv$status}) ,
    WH=reactive({rtv$WH})
  )
  
}
