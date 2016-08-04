
  modulePlotSVGrUI <- function(id, input, output) { 
    ns <- NS(id)
    absolutePanel( top=130, left=0, right=0,  draggable=FALSE,
                     style=cstyle$svg, htmlOutput(ns( "svghtml" ))
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
    getCodeBackup,
    getErrorMssg, 
    insert.end #='showPts.compound()'
  ){
    
  user<-reactiveValues( code="" )
  backup<-reactiveValues(code="")
  mssg<-reactiveValues( error=NULL )
  
  
  
  output$svghtml <- renderUI({
    
    WH<-getSVGWH()
    #src<-getCode()
    src<-getCode2()
    backup$code<-getCodeBackup()
    if(is.null(showGrid())){return(NULL)}
    
    # why can't I force this???
    showPts.compound=showPts.compound #should be able to force this
    svgid<-paste0('id="', svgID, '",')
    #defining the prolog 
    insert.beg<-c( svgid,
      'style(".draggable {','cursor: move;','}"),', 
      gsub('ptrDisplayScript', ptrDisplayScript, "script('ptrDisplayScript'),"),      
      "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),",
      if(showGrid()==TRUE){"graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),"} 
      else { NULL }
    )
  
    #defining the epilog
    #put it together
    src<-subSVGX2(src, insert.beg, insert.end)
    
    # transform: modifies src, but omits insert.end
   
    res<-""
    tryCatch({
        parsedCode<-parse(text=src)
        svg<-eval(parsedCode)
        as.character(svg)->svgOut 
        res<-HTML(svgOut)
        backup$code<-getCode()
        mssg$error<-NULL
      },
      error=function(e){
        # session$sendCustomMessage(type='testmessage', message=e)
        #mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
        user$code<-getCodeBackup()
        mssg$error<-paste(getErrorMssg(), e, collapse="\n", sep="\n")
        print(e)
        #updateNavbarPage(session, "plotNavBar", selected ="Log")
      } 
    )
    res
  }) #end of renderUI
  
  list(
    code=reactive({user$code}),
    backup=reactive({backup$code}),
    mssg=reactive({mssg$error})
  )
  
}
