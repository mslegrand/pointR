
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
    useKeyMouseScript, #  
    getSVGWH, 
    getSvgGrid,
    getBackDrop,
    getCode,
    getErrorMssg, 
    getTibNRow, # doesnot appear
    getDirPath
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
 
  graphPaper2 %<c-% function(wh, dxy=c(10, 10), labels=FALSE, scaleFactor =1 ){
    seq(0,wh[1],dxy[1])->xs
    seq(0,wh[2],dxy[2])->ys
    grph<-g(
      stroke.width=1/scaleFactor,
      lapply(xs, function(x){line(xy1=c(x,0),xy2=c(x,wh[2]))}),
      lapply(ys, function(y){line(xy1=c(0,y),xy2=c(wh[1],y))})
    )
    if(labels){
      grph<-c(grph,
              lapply(xs, function(x)text(font.size=10, 
                                         xy=scaleFactor*c(x,0)+c(5,5),
                                         x, 
                                         text.anchor="start", alignment.baseline="hanging" ,
                                         transform=paste0('scale(',1/scaleFactor ,')') 
                                         )
                     ),
              lapply(ys, function(y)text(font.size=10, 
                                         xy=scaleFactor*c(0,y)+c(5,-5),
                                         y, 
                                         text.anchor="start", alignment.baseline="baseline",
                                         transform=paste0('scale(',1/scaleFactor ,')') ))
      )
    }
    g( 
       font.size=10,
       stroke="grey",
       #transform=paste0('scale(',1/scaleFactor,')' ),
       grph
    )
  }
 
  
  output$svghtml <- renderUI({ # renderUI is probably not the most efficient approach!!!
    WH<-getSVGWH()
    codeTxt<-getCode()
    if(is.null(getSvgGrid())){return(NULL)}
    
    # 
    showPts.compound=showPts.compound #should be able to force this
    svgid<-paste0('id="', svgID, '",')
    ptrDisplyScriptTxt<-unlist(ptrDisplayScript())

    # transform: modifies src, but omits insert.end
    res<-""
    if(!is.null(codeTxt)){
      tryCatch({
        # Set wd to the current project or if no project, then to home
          dpath<-getDirPath()
          if(identical(dpath, '~/.ptR')){
            dpath<-'~'
          }
          wd<-paste0('\nsetwd("',dpath,'")\n\n')
          
          parsedCode<-parse(text=paste0(wd,codeTxt))
          svg<-eval(parsedCode, new.env() )
          w<-svg$root$getAttr('width')
          h<-svg$root$getAttr('height')
          rtv$WH<-c(w,h)
          vbWH<-svg$root$getAttr('viewBox')
          vbWH<-str_split(vbWH,',')
          vbWH<-unlist(vbWH)[3:4]
          vbScaleFactor<-1
          gWH<-c(w,h)
          tryCatch({
            if(length(vbWH)==2  ){
              vbWH<-as.numeric(vbWH)
              if(min(vbWH)>0){
                vbScaleFactor<-mean(rtv$WH/vbWH)
                gWH<-vbWH
              } else {
                vbScaleFactor<-1
              }
            } 
            }, error=function(e){
              vbScaleFactor<-1
          }) 
          svg$root$setAttr('id',svgID)
          if(getSvgGrid()$show==TRUE){ 
            dxy<-c( getSvgGrid()$dx, getSvgGrid()$dy)
            
            #svg$root$prependNode(svgR:::graphPaper( wh=c(w,h), dxy=dxy, labels=TRUE )) #need to replace with vbScaleFactor-scalable version
            #svg$root$prependNode( graphPaper2( wh=c(w,h), dxy=dxy, labels=TRUE, scaleFactor= vbScaleFactor))
            svg$root$prependNode( graphPaper2( wh=gWH, dxy=dxy, labels=TRUE, scaleFactor= vbScaleFactor))
          }
          if(getBackDrop()$checked==FALSE){
              svg$root$prependChildren(
                svgR:::use(filter=svgR:::filter(xy=c(0,0), wh=c(w,h), filterUnits="userSpaceOnUse", svgR:::feFlood(flood.color=getBackDrop()$color)))
              )
          } else {
             wh2=c(20,20)/vbScaleFactor
             wh1=c(10,10)/vbScaleFactor
             svg$root$prependChildren(
               svgR:::rect(xy=c(0,0), wh=c(w,h),
                  fill=svgR:::pattern( xy=c(0,0), wh=wh2, patternUnits="userSpaceOnUse",
                    svgR:::g(
                      svgR:::rect(xy=c(0,0), wh=wh1, fill=getBackDrop()$color),
                      svgR:::rect(xy=wh1, wh=wh1, fill=getBackDrop()$color)
                    )
                  )
                )
              )
          }
          svg$root$prependNode(svgR:::script(ptrDisplyScriptTxt))
          svg$root$prependNode( svgR:::style(".draggable {','cursor: move;','}"))
            
          if(!is.null(showPts.compound()) ){
              temp<-svgR(showPts.compound()(vbScaleFactor))$root$xmlChildren()
              svg$root$appendChildren(temp)
          }
          if(useKeyMouseScript){
            # Append to svg$root the attribute onmousedown
            keyMouseScript=paste0('onKeyMouseDown(evt, "', svgID, '")')
            svg$root$addAttributes(list(onmousedown=keyMouseScript))
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
