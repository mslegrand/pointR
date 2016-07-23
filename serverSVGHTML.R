#----SVG window-------------------

# workflow:
# 1 get svgBarCmd
# 2 get points, indx
# 3 get tags, tag.indx
#    load jjscript
# 4 form showPts
# 5 form prolog
# 6 form epilog


output$svghtml <- renderUI({
  svgBarCmd<-input$plotNavBar
  WH<-c(650,620)
  if(svgBarCmd=="Log"){
    return("")
  }
   
#  getPoints<-function( ){
#    # ptName<-selectedPoint$name
#    ptRList<-getPtDefs()$pt
#    # pts<-NULL
#    #selectedPointIndx<-NULL
#    if( !is.null( selectedPoint$name ) && !is.null(ptRList)){
#      pts<- ptRList[[ selectedPoint$name ]]
#    } else {
#      pts<-NULL
#    }
#  }
  
  ptName<-selectedPoint$name
  ptRList<-getPtDefs()$pt
  ptDisplayMode<-displayOptions$ptMode
  
  pts<-NULL
  #selectedPointIndx<-NULL
#  if(svgBarCmd!="Transforms" && !is.null(ptName) && !is.null(ptRList)){
#    pts<- ptRList[[ptName]]
#    selectedPointIndx<-selectedPoint$point.index
#  }
  
#  showPtOptions<-switch( svgBarCmd ){
#    Points= list(ptDisplayMode=displayOptions$ptMode ),
#  }
  
 # preperation: pick script, get dependent vbls 
  if(svgBarCmd=="Points"){
#    ptDisplayMode<-displayOptions$ptMode
    #ptDisplayMode<-showOptions$showPoints
    scriptName<-"Points"
#    ptTags<-NULL #tagRList[[ptName]]  #we donot show tags here
#    tag.indx<-NULL
#    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){
#        pts<-NULL
#    }
#    showPtOptions<-list(
#      ptDisplayMode=ptDisplayMode, 
#      ptTags=ptTags,
#      selectedPointIndx=selectedPoint$point.indx,
#      tag.indx=tag.indx
#    )
    #print(showPtOptions$selectedPointIndx)
  } 
  
  if(svgBarCmd=="tagValues"){
    #ptDisplayMode<-input$ptDisplayMode
#    ptDisplayMode<-displayOptions$ptMode #showOptions$showPoints
    if(ptDisplayMode=="hidden"){
      ptDisplayMode<-"normal"
    }
    scriptName<-"Points"
    #tag.indx<-as.numeric(input$tagIndx) 
#    tag.indx<-selectedPoint$point.index
#    tagRList<-getPtDefs()$df 
#    if(!is.null(tagList)){
#      ptTags<-tagRList[[ptName]]
#    } else {
#      ptTags<-NULL
#    }
#    showPtOptions<-list(
#      ptDisplayMode=ptDisplayMode, 
#      ptTags=ptTags,
#      selectedPointIndx=selectedPoint$point.index,
#      tag.indx=tag.indx
#    )
  }
  
  if(svgBarCmd=="dragTag"){
    #ptDisplayMode<-input$ptDisplayMode #do not use here
    ptDisplayMode<-"normal" #showOptions$showPoints
    scriptName<-"transTag"
    #scriptName<-"Points"
    #tag.indx<-as.numeric(input$tagIndx)
#    tag.indx<-selectedPoint$point.index
#    #tag.indx<-as.numeric(input$tagIndx2)
#    tagRList<-getPtDefs()$df 
#    if(!is.null(tagRList)){
#      ptTags<-tagRList[[ptName]]
#    } else {
#      ptTags<-NULL
#    }
#    
#    showPtOptions<-list(
#      ptDisplayMode=ptDisplayMode, 
#      ptTags=ptTags,
#      selectedPointIndx=selectedPointIndx,
#      tag.indx=tag.indx
#    )
  }
  
#  if(svgBarCmd=="Transforms"){ #Temp kludge for transform)
#    ptName<-NULL
#    pts<- NULL
#    ptTags<-NULL
#    scriptName<-input$transformOption
#  } 
  
  #showGrid<-input$showGrid
  showGrid<-displayOptions$showGrid
  if(is.null(showGrid)){
    return(NULL)
  }
  ptrDisplayScript<-js.scripts[[ scriptName]]
  src<-user$code
  if(svgBarCmd=="Transforms"){
    src<-usingDraggable(src)
  }
  
  
  # called when we need to show points (in epilog)
  # to do: rewrite to make this work with call for tags
  # where each tag is a group, so that we can edit a tag set 
  # to provide ability for translate, rotate, scale of points
#  showPts.olde %<c-% function(pts, ptTags, showPtOptions=NULL){
#    if(is.null(pts) ){
#      return(NULL)
#    } 
#    pts<- ptRList[[ptName]]
#    
#    if(length(pts)<2 ){ #do we still need this?????
#      return(NULL)
#    }
#    tag.indx<-showPtOptions$tag.indx
# 
#    semitransparent<-0.3
#    colorScheme<-c(default="green", ending="red", selected="blue")
#    m<-matrix(pts,2) # is this really necessary????
#     
#    #preproc for tagList 
#    if(!is.null(ptTags) && !is.null(tag.indx) ){
#      tags<-ptTags$tag
#      ti<-which(tag.indx==tags)
#      tagInterval<-findInterval(sequence(ncol(m)),tags)
#      tagInterval<-tagInterval==ti
#      tagInterval[tagInterval==0]<-semitransparent
#      opac<-tagInterval
#      selectedPointIndx<-0 # so tags will not show selectes
#    } else {
#      opac<-rep(1, ncol(m) )
#    }
#    #form list of  all point renderings
#    lapply(1:ncol(m), function(i){
#      id<-paste("pd",ptName,i,sep="-")
#      pt<-m[,i]
#      color=colorScheme['default']
#      if(i==length(pts)/2) { #ncol(m)){
#          color=colorScheme['ending']   
#      }
#      list(
#        if(i==selectedPointIndx && is.null(tag.indx)){ #show selected point for points mode (or transform???)
#          circle(class="draggable", 
#                 id=id,  
#                 cxy=pt, r=9, fill=color, 
#                 opacity=opac[i],
#                 stroke=colorScheme['selected'], stroke.width=3,
#                 transform="matrix(1 0 0 1 0 0)", 
#                 onmousedown="selectPoint(evt)" )
#        } else { #a non-selected point
#          circle(class="draggable", 
#                 id=id,  
#                 cxy=pt, r=8, fill=color, opacity=opac[i],
#                 transform="matrix(1 0 0 1 0 0)", 
#                 onmousedown="selectPoint(evt)" )
#        },
#        if(showPtOptions$ptDisplayMode=="Labeled"){
#        #if(input$ptDisplayMode=="Labeled"){
#            text(paste(i), cxy=pt+10*c(1,-1),  
#               stroke='black', font.size=12, opacity=1) #opac)
#        } else {
#          NULL
#        }
#      )
#    }) #end lapply
#  } #end showPts.oldeß
#  
  showPts.transform %<c-% function(){ NULL }
  
  showPts.valTag%<c-% function(pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  ptTags=NULL){
  #showPts.valTag %<c-% function(pts, ptTags, showPtOptions){
    #cat(file=stderr(),"entering drag.Tag\n")
    if( is.null(pts) ){return(NULL)} 
    if(length(pts)<2){return(NULL)}
    print("entering val.Tag\n")
    #tag.indx<-showPtOptions$tag.indx #this is the position of the first point of the tagged set 
    tag.indx<-selectedPointIndx
    #selectedPointIndx<-showPtOptions$selectedPointIndx
    semitransparent<-0.3
    colorScheme<-c(default="green", ending="red", selected="blue")
    
    m<-matrix(pts,2)
    if( is.null(tag.indx) ){ stop("unexpected null for tag.indx") }
    if( is.null(ptTags)   ){ stop("ptTags is null") }

    tags<-ptTags$tag
    ti<-which(tag.indx==tags) 
    tagInterval<-findInterval(sequence(ncol(m)),tags)
    tagInterval<-tagInterval==ti
    tagInterval[tagInterval==0]<-semitransparent
    opac<-tagInterval
      
    # iterate over tagIntList
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
        color=colorScheme['ending']   
      }
      list(
        circle(class="draggable", 
          id=id,  
          cxy=pt, r=8, fill=color, opacity=opac[i],
          transform="matrix(1 0 0 1 0 0)", onmousedown="selectPoint(evt)" 
        ),
        if(ptDisplayMode=="Labeled"){
          text(paste(i), cxy=pt+10*c(1,-1),  stroke='black', font.size=12, opacity=opac[i]) #opac)
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts

  
  # called when we need to show points (in epilog)
  # to do: rewrite to make this work with call for tags
  # where each tag is a group, so that we can edit a tag set 
  # to provide ability for translate, rotate, scale of points
  showPts.PtCmd %<c-% function(pts=NULL,  selectedPointIndx=NULL, ptDisplayMode="Normal"){
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    #pts<- ptRList[[ptName]]
    if(length(pts)<2 ){ return(NULL)}
    print("entering showPts.PtCmd")
    selectedPointIndx<-as.numeric(selectedPointIndx)
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    m<-matrix(pts,2) # is this really necessary????
     
    #form list of  all point renderings
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
          color=colorScheme['ending']   
      }
      list(
        if(identical(selectedPointIndx, as.numeric(i) )){
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=9, fill="yellow", 
                 opacity=1,
                 stroke=colorScheme['selected'], stroke.width=3,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        } else { #a non-selected point
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, opacity=1,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        },
        if(ptDisplayMode=="Labeled"){
            text(paste(i), cxy=pt+10*c(1,-1),  
               stroke='black', font.size=12, opacity=1) 
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts.PtCmd
  
  # called when we need to show points
# to do: rewrite to make this work with call for tags
# where each tag is a group, so that we can edit a tag set 
# to provide ability for translate, rotate, scale of points

# dragtag, magtag, wagtag, zagtag ? bagtag?

  showPts.dragTag %<c-% function(pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  tags=NULL){
    #cat(file=stderr(),"entering drag.Tag\n")
    print("entering drag.Tag")
    if( is.null(pts) ) {return(NULL) } 
    if(length(pts)<2)  {return(NULL) }
    # browser()
    tag.indx<-selectedPointIndx #this is the position of the first point of the tagged set 
    # print(tag.indx)
    # print(tags)
    semitransparent<-0.3
    colorScheme<-c(default="purple", ending="red", selected="blue")
    color<-colorScheme[1]
    m<-matrix(pts,2)
    if( !is.null(tag.indx) && !is.null(tags)){
      #print(paste("ptTags", ptTags, collapse=", "))
      #print("class (ptTags)=",class(ptTags))
      # print("class (tag.indx)=")
      # print(class(tag.indx))
      # print("length(tag.indx)=")
      # print(length(tag.indx))
      #tags<-as.numeric(ptTags$tag)
      # print("class (tags)=")
      # print(class(tags))
      ti<-which(max(tags[tags<=tag.indx])==tags )
      
      # print("length(ti)=", length(ti))
      # print("ti=")
      # print(ti)
      id.nos<-sequence(ncol(m))
      ids<-paste("pd",ptName,id.nos,sep="-")
      tagInterval<-findInterval(id.nos,tags)
      tagIntList<-tapply(id.nos, findInterval(id.nos,tags), list )
      opacity<-rep(semitransparent, length(tags))
      opacity[ti]<-1
      # browser()
      # iterate over tagIntList
      indx<-unique(tagInterval)
      indx<-indx[-ti]
      list(
        g( opacity=opacity[ti], 
           #class='draggable', #this is for dragtag
           fill='purple',
           transform="matrix(1 0 0 1 0 0)", 
           onmousedown="selectElement(evt)",
           tid=paste0("ptR_Tag_",ti),
           lapply(tagIntList[[ti]], function(j){
             # print("one")
             # print(j)
             circle(   cxy=m[,j], r=8)
           })
        ),
        lapply(indx, function(i){
          #browser()
          g( opacity=opacity[i], 
             #class='draggable', #this is for dragtag
             fill='purple',
             transform="matrix(1 0 0 1 0 0)", 
             #onmousedown="selectElement(evt)",
             tid=paste0("ptR_Tag_",i),
             # {
             #  print(paste("tid=",tid))
             #  print(tagIntList[[i]])
             #  NULL
             # },
             # 
             lapply(tagIntList[[i]], function(j){
               # print("two")
               # print(j)
               circle(   cxy=m[,j], r=8)
             })
          )
        })
      )
      } #end if
  } #end showPts

  
  showPts<-function(ptName, ptRList, ptDisplayMode=NULL){
    if(!is.null(ptName) && !is.null(ptRList)){
      pts<- ptRList[[ptName]]
      selectedPointIndx<-as.numeric( selectedPoint$point.index )
    } else {
      pts<-NULL
      selectedPointIndx<-0
    }
    switch(svgBarCmd,
      Points     = {  
        #ptDisplayMode<-displayOptions$ptMode
        showPts.PtCmd(pts=pts,  selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode)
      },
      tagValues    = { 
        tagRList<-getPtDefs()$df 
        if(!is.null(tagList)){
          ptTags<-tagRList[[ptName]]
        } else {
          ptTags<-NULL
        }
        showPts.valTag(pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  ptTags=ptTags)
      },      
      dragTag    = { 
        #browser()
        tagRList<-getPtDefs()$df 
        if(!is.null(tagRList)){
          tags<-tagRList[[ptName]]$tag
        } else {
          tags<-NULL
        }
        showPts.dragTag(pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  tags=tags)
      },
      
      #tagValues  = showPts.valTag(pts, ptTags, showPtOptions),
      Transforms = NULL,
      NULL
    )
  }
  
  #Only applies when svgBarCmd=="Points"
  newPtLayer %<c-% function(svgBarCmd, wh=c(1200,800)){
    if(svgBarCmd=="Points" && displayOptions$insertMode==TRUE){
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
  
  #print(displayOptions$showGrid)
  if( displayOptions$showGrid==TRUE){
    insert.beg<-c(insert.beg, "graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),")
    insert.beg
  } 
  
  #defining the epilog
  insert.end<-c(
    ',newPtLayer(svgBarCmd, WH),',
    'showPts(ptName, ptRList,  ptDisplayMode)'
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
        # session$sendCustomMessage(type='testmessage', message=e)
        mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
        # cat(mssg$error)
        user$code<-backup$code
        updateNavbarPage(session, "plotNavBar", selected ="Log")
      } 
    )
  res
})
