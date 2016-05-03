#----SVG window-------------------
output$svghtml <- renderUI({
  svgBarCmd<-input$plotNavBar
  WH<-c(600,620)
  if(svgBarCmd=="Log"){
    return("")
  }
  
  
  if(svgBarCmd=="Points"){
    ptName<-input$ptRSelect
    ptRList<-getPtDefs()$pt
    selectedPointIndx<-selectedPoint$point.index
    ptDisplayMode<-input$ptDisplayMode
    scriptName<-"Points"
    #todo use input$pointOption :
    # pointOpt=c("Insert", "Edit","Tag")
    tag.indx<-NULL
    showPtOptions<-list(ptDisplayMode=ptDisplayMode, tag.indx=NULL)
  } 
  if(svgBarCmd=="Tags"){
    ptName<-input$ptRSelect
    ptRList<-getPtDefs()$pts
    selectedPointIndx<-selectedPoint$point.index
    ptDisplayMode<-input$ptDisplayMode
    scriptName<-"Points"
    #todo use input$pointOption :
    # pointOpt=c("Insert", "Edit","Tag")
    tag.indx<-input$tagIndx
    showPtOptions<-list(ptDisplayMode=ptDisplayMode, tag.indx=tag.indx)
  }
  if(svgBarCmd=="Transforms"){ #Temp kludge for transform)
    ptName<-NULL
    scriptName<-input$transformOption
  } 
  
    
  
  showGrid<-input$showGrid
  
  script2<-js.scripts[[ scriptName]]
  src<-user$code
  src<-usingDraggable(src)
  
  showPts %<c-% function(ptName,  showPtOptions=NULL){
    if(is.null(ptName) || is.null(showPtOptions) 
       || 
       (showPtOptions$ptDisplayMode=="Hidden" && 
        is.null(showPtOptions$tag.indx))
       ){
      return(NULL)
    }  
    tag.indx<-showPtOptions$tag.indx
   
    # print(paste0("selectedPointIndx=",selectedPointIndx)) #TODO: move this out!!!
    # print(paste0("tag.indx=",tag.indx))
    # print(paste0("class(tag.indx)=",class(tag.indx)))
    # 

    semitransparent<-0.3
    colorScheme<-c(default="green", ending="red", selected="blue")
    
    
    ptRList<-getPtDefs()$pts #TODO: move this out!!!
    
    pts<- ptRList[[ptName]]
    tagRList<-getPtDefs()$df #TODO: move this out!!!
    
    if(length(pts)<2){
      return(NULL)
    } else{
      m<-matrix(pts,2)
      if(!is.null(tagList) && 
         !is.null(tag.indx) && 
         !is.null(tagRList[[ptName]] 
      )){
        tags<-tagRList[[ptName]]$tag
      } else {
        tags<-c()
        tag.indx<-0
      }
      if(ncol(m)>0){
        tagsX<-c(0,tags,ncol(m)+1) # has at least 2 elements
        t1<-max(tagsX[tagsX<=tag.indx])
        t2<-min(tagsX[tag.indx<tagsX])
      } else {
        t1=0; t2=1000
      }
      lapply(1:ncol(m), function(i){
        id<-paste("pd",ptName,i,sep="-")
        pt<-m[,i]
        color=colorScheme['default']
        if(i==length(pts)/2) { #ncol(m)){
            color=colorScheme['ending']   
        }
        if( t1<=i && i<t2 ){ 
          opac<-1 
        } else {
          opac<-semitransparent
        }
        list(
          if(i==selectedPointIndx && svgBarCmd!="Tags"){
            circle(class="draggable", 
                   id=id,  
                   cxy=pt, r=9, fill="yellow", 
                   opacity=opac,
                   stroke=colorScheme['selected'], stroke.width=3,
                   transform="matrix(1 0 0 1 0 0)", 
                   onmousedown="selectPoint(evt)" )
          } else {
            circle(class="draggable", 
                   id=id,  
                   cxy=pt, r=8, fill=color, opacity=opac,
                   transform="matrix(1 0 0 1 0 0)", 
                   onmousedown="selectPoint(evt)" )
          }
          ,
          
          if(showPtOptions$ptDisplayMode=="Labeled"){
            text(paste(i), cxy=pt+10*c(1,-1),  
                 stroke='black', font.size=12, opacity=opac)
          } else {
            NULL
          }
        )
      })
    }
  }
  
  newPtLayer %<c-% function(svgBarCmd, wh=c(1200,800)){
    if(svgBarCmd=="Points" && input$insertMode==TRUE){
      rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
    } else {
      NULL
    } 
  }
  
  boundingBox %<c-% function(){ #not used!!! may consider to use in future
    if(svgBarCmd=="rotate"){
      rect(id='x-bdd-rect', cxy=WH/2, wh=WH/4, stroke='red',fill='none', opacity=.5)
    } else {
      NULL
    }
  }
  
  
  insert.beg<-c( 
    'style(".draggable {','cursor: move;','}"),', 
    gsub('script2', script2, "script('script2'),"),      
    "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),"
  )
  if(showGrid==TRUE){
    insert.beg<-c(insert.beg, "graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),")
  }
  
  insert.end<-c(
    #paste(',newPtLayer("',svgBarCmd,'"),'),
    ',newPtLayer(svgBarCmd, WH),',
    'showPts(ptName,  showPtOptions)'
    #boundingBox()
  )    
  
  src<-subSVGX2(src, insert.beg, insert.end)
  res<-""
    tryCatch({
        parsedCode<-parse(text=src)
        svg<-eval(parsedCode)
        as.character(svg)->svgOut 
        res<-HTML(svgOut)
        backup$code<-user$code
        backup$res<-res
      },
      error=function(e){
        #session$sendCustomMessage(type='testmessage', message=e)
        mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
        user$code<-backup$code
        updateNavbarPage(session, "plotNavBar", selected ="Log")
      } 
    )
  res
 
})
