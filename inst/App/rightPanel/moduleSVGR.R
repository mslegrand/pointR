
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
    absolutePanel( 
                   "class"="cSvgHtml", 
                    draggable=FALSE,
                    htmlOutput(ns( "svghtml" ), inline=FALSE)
    )
  }
  
  modulePlotSVGr<-function(
    input, output, session,
    svgID, #='ptR_SVG_Point'
    showPts.compound, # =showsvgRPoints.pts2
    ptrDisplayScript, # =js.scripts[[ "Points"]]
    getSVGWH, 
    showGrid,
    getCode,
    getCode2,  # =getCode (or getCodeTransform)
    getErrorMssg, 
    insert.end #='showPts.compound()'
  ){
    
  user<-  reactiveValues( code="")
  
  rtv<-  reactiveValues(
    status=list(
      state="PASS", 
      message=""
    )
  )
 
  
  # Todo: add the mouseMssg handler
  
  output$svghtml <- renderUI({
    WH<-getSVGWH()
    #src<-getCode()
    codeTxt<-getCode2()
    if(is.null(showGrid())){return(NULL)}
    
    # why can't I force this???
    showPts.compound=showPts.compound #should be able to force this
    svgid<-paste0('id="', svgID, '",')
    #defining the prolog 
    
    ptrDisplyScriptTxt<-ptrDisplayScript()
    insert.beg<-c( svgid,
      'style(".draggable {','cursor: move;','}"),', 
      gsub('ptrDisplayScript', ptrDisplyScriptTxt, "script('ptrDisplayScript'),"),      
      "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),",
      if(showGrid()==TRUE){"graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),"} 
      else { NULL }
    )
    #defining the epilog
    #put it together
   
    
     
     
    codeTxt<-subSVGX2(codeTxt, insert.beg, insert.end)
    
    
    # transform: modifies src, but omits insert.end
    res<-""
    if(!is.null(codeTxt)){
      tryCatch({
          parsedCode<-parse(text=codeTxt)
          svg<-eval(parsedCode)
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
    status=reactive({rtv$status}) 
  )
  
}
