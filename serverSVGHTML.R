#----SVG window-------------------

# workflow:
# 1 get svgBarCmd
# 2 get points, indx
# 3 get tags, tag.indx
#    load jjscript
# 4 form showPts
# 5 form prolog
# 6 form epilog
# 7 
output$svghtml <- renderUI({
  svgBarCmd<-input$plotNavBar
  WH<-c(650,620)
  if(svgBarCmd=="Log"){
    return("")
  }
    ptName<-input$ptRSelect
    ptRList<-getPtDefs()$pt
    pts<-NULL
    if(svgBarCmd!="Transforms" && !is.null(ptName) && !is.null(ptRList)){
      pts<- ptRList[[ptName]]
      selectedPointIndx<-selectedPoint$point.index
    }
  
 # preperation: pick script, get dependent vbls 
  if(svgBarCmd=="Points"){
    ptDisplayMode<-input$ptDisplayMode 
    scriptName<-"Points"
    ptTags<-NULL #tagRList[[ptName]]  #we donot show tags here
    tag.indx<-NULL
    if(ptDisplayMode=="Hidden"){
        pts<-NULL
    }
    showPtOptions<-list(
      ptDisplayMode=ptDisplayMode, 
      ptTags=ptTags,
      selectedPointIndx=selectedPointIndx,
      tag.indx=tag.indx
    )
  } 
  
  if(svgBarCmd=="tagValues"){
#    ptName<-input$ptRSelect
#    ptRList<-getPtDefs()$pts
#    pts<- ptRList[[ptName]]
#    
#    selectedPointIndx<-selectedPoint$point.index
    ptDisplayMode<-input$ptDisplayMode
    scriptName<-"Points"
    tag.indx<-as.numeric(input$tagIndx)
    tagRList<-getPtDefs()$df 
    if(!is.null(tagList)){
      ptTags<-tagRList[[ptName]]
    } else {
      ptTags<-NULL
    }
    showPtOptions<-list(
      ptDisplayMode=ptDisplayMode, 
      ptTags=ptTags,
      selectedPointIndx=selectedPointIndx,
      tag.indx=tag.indx
    )
  }
  if(svgBarCmd=="dragTag"){
    ptDisplayMode<-input$ptDisplayMode #do not use here
    scriptName<-"transTag"

    tag.indx<-as.numeric(input$tagIndx)
    tagRList<-getPtDefs()$df 
    if(!is.null(tagList)){
      ptTags<-tagRList[[ptName]]
    } else {
      ptTags<-NULL
    }
    
    showPtOptions<-list(
      ptDisplayMode=ptDisplayMode, 
      ptTags=ptTags,
      selectedPointIndx=selectedPointIndx,
      tag.indx=tag.indx
    )
  }
  if(svgBarCmd=="Transforms"){ #Temp kludge for transform)
    ptName<-NULL
    pts<- NULL
    ptTags<-NULL
    scriptName<-input$transformOption
  } 
  
  showGrid<-input$showGrid
  
  ptrDisplayScript<-js.scripts[[ scriptName]]
  src<-user$code
  src<-usingDraggable(src)
  
  # called when we need to show points (in epilog)
  # to do: rewrite to make this work with call for tags
  # where each tag is a group, so that we can edit a tag set 
  # to provide ability for translate, rotate, scale of points
  showPts %<c-% function(pts, ptTags, showPtOptions=NULL){
    if(is.null(pts) ){
      return(NULL)
    } 
    pts<- ptRList[[ptName]]
    
    if(length(pts)<2 ){ #do we still need this?????
      return(NULL)
    }
    tag.indx<-showPtOptions$tag.indx
 
    semitransparent<-0.3
    colorScheme<-c(default="green", ending="red", selected="blue")
    m<-matrix(pts,2) # is this really necessary????
     
    #preproc for tagList 
    if(!is.null(ptTags) && !is.null(tag.indx) ){
      tags<-ptTags$tag
      ti<-which(tag.indx==tags)
      tagInterval<-findInterval(sequence(ncol(m)),tags)
      tagInterval<-tagInterval==ti
      tagInterval[tagInterval==0]<-semitransparent
      opac<-tagInterval
      #selectedPointIndx<-0 #so tags will not show selectes
    } else {
      opac<-rep(1, ncol(m) )
    }
    
    #form list of  all point renderings
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
          color=colorScheme['ending']   
      }
      list(
        if(i==selectedPointIndx && is.null(tag.indx)){ #show selected point for points mode (or transform???)
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=9, fill="yellow", 
                 opacity=opac[i],
                 stroke=colorScheme['selected'], stroke.width=3,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        } else { #a non-selected point
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, opacity=opac[i],
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        },
        if(showPtOptions$ptDisplayMode=="Labeled"){
          text(paste(i), cxy=pt+10*c(1,-1),  
               stroke='black', font.size=12, opacity=1) #opac)
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts
  
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
  
  #defining the prolog 
  insert.beg<-c( 
    'style(".draggable {','cursor: move;','}"),', 
    gsub('ptrDisplayScript', ptrDisplayScript, "script('ptrDisplayScript'),"),      
    "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),"
  )
  
  if(showGrid==TRUE){
    insert.beg<-c(insert.beg, "graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),")
  }
  
  #defining the epilog
  insert.end<-c(
    #paste(',newPtLayer("',svgBarCmd,'"),'),
    ',newPtLayer(svgBarCmd, WH),',
    'showPts(pts, ptTags,  showPtOptions)'
    #boundingBox()
  )    
  
  #put it together
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
