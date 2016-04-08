#----SVG window-------------------
output$svghtml <- renderUI({
  svgBarCmd<-input$plotNavBar
  WH<-c(600,620)
  if(svgBarCmd=="Points"){
    ptName<-input$ptSet
    ptRList<-getPtDefs()$pts

    selectedPointIndx<-selectedPoint$point.index
    scriptName<-"Points"
    #todo use input$pointOption :
    # pointOpt=c("Insert", "Edit","Tag")
    tag.indx<-NULL
  } 
  if(svgBarCmd=="Tags"){
    ptName<-input$ptSet
    ptRList<-getPtDefs()$pts
    selectedPointIndx<-selectedPoint$point.index
    scriptName<-"Points"
    #todo use input$pointOption :
    # pointOpt=c("Insert", "Edit","Tag")
    tag.indx<-input$tagIndx
  }
  if(svgBarCmd=="Transform"){ #Temp kludge for transform)
    ptName<-NULL
    scriptName<-input$transformOption
  } 
  
  
  
  showGrid<-input$showGrid
  
  script2<-js.scripts[[ scriptName]]
  src<-user$code
  src<-usingDraggable(src)
  
  showPts %<c-% function(ptName, tag.indx=NULL){
    if(is.null(ptName)){
      return(NULL)
    }  
    
    semitransparent<-.3
    colorScheme<-c(default="blue", ending="red", current="green")
    ptRList<-getPtDefs()$pts
    
    
    pts<- ptRList[[ptName]]
    tagRList<-getPtDefs()$df
    
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
        tags<-c(0,tags,ncol(m)+1)
        t1<-max(tags[tags<=tag.indx])
        t2<-min(tags[tag.indx<tags])
      } else {
        t1=0; t2=1000
      }
            
      
      lapply(1:ncol(m), function(i){
        id<-paste("pd",ptName,i,sep="-")
        pt<-m[,i]
        color=colorScheme['default']
        if(i==selectedPointIndx){
          color=colorScheme['current']      
        } else{
          if(i==length(pts)/2) { #ncol(m)){
            color=colorScheme['ending']   
          }
        } 
        if( t1<=i && i<t2 ){ 
          opac<-1 
        } else {
          opac<-semitransparent
        }
        circle(class="draggable", 
               id=id,  
               cxy=pt, r=8, fill=color, opacity=opac,
               transform="matrix(1 0 0 1 0 0)", 
               onmousedown="selectPoint(evt)" )
      })
    }
  }
  
  newPtLayer %<c-% function(svgBarCmd, wh=c(1200,800)){
    if(svgBarCmd=="Points" ){
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
    insert.beg<-c(insert.beg, "graphPaper( wh=c(1200,1200), dxy=c(50, 50), labels=TRUE ),")
  }
  
  insert.end<-c(
    #paste(',newPtLayer("',svgBarCmd,'"),'),
    ',newPtLayer(svgBarCmd, WH),',
    'showPts(ptName, tag.indx)'
    #boundingBox()
  )    
  
  src<-subSVGX2(src, insert.beg, insert.end)
  svg<-eval(parse(text=src))
  
  
  
  
  as.character(svg)->svgOut 
  HTML(svgOut)
})
