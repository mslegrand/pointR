
# --------------input$plotNavBar=="dragTag"---------------- 

 output$TagDragPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='tagDrag'", moduleTagDragUI("tagDragBar"))
 })

tagDragInfoList<-callModule(
  module=moduleTagDrag,
  id="tagDragBar",
  barName=reactive(input$plotNavBar),
  getTagNameChoices=getTagNameChoices,
  getTagName=getTagName,
  getTagIndexChoices=getTagIndexChoices,
  getTagIndex=getTagIndex
)

observe({
  name<-tagDragInfoList$name()
  index<-tagDragInfoList$index()
#  isolate({
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
#  })
})

observeEvent( 
  tagDragInfoList$tagClone(),
  {
    
    name<-tagDragInfoList$name()
    index<-tagDragInfoList$index()
    ptRList<-getPtDefs()$pts
    tagRList<-getPtDefs()$df
    pts<-getPtDefs()$pts[[name]] 
    df<-tagRList[[name]]
    tags<-df$tag
    ti<-which(index==tags) 
    id.nos<-sequence(ncol(pts))
    tagInterval<-findInterval(id.nos,tags)
    ptsA<-pts[,tagInterval<=ti]
    ptsB<-pts[,tagInterval>=ti]
    
    tiSize<-ncol(pts[,tagInterval==ti])
    
    ptsNew<-matrix(c(ptsA,ptsB),2)
    t2<-tags[tags>=index]+tiSize
    t1<-tags[tags<=index]
    tagsNew<-c(t1,t2)
    
    df1<-subset(df,df$tag<=index)
    df2<-subset(df,df$tag>=index)
    dfNew<-as.data.frame(rbind(df1,df2))
    dfNew$tag<-tagsNew
    ptRList[[name]]<-ptsNew
    tagRList[[name]]<-dfNew
    scr<-getCode()
    src<-user$code
    src<-pts2Source(src,ptRList)
    src<-df2Source( src, tagRList)
    
    #update
    user$code<-src
    selectedPoint$point.index<-as.numeric(index)+tiSize
  }
)

#delete tag set
observeEvent( 
  tagDragInfoList$tagDelete(),
  {
    name<-    tagDragInfoList$name()
    index<-   tagDragInfoList$index()
    ptRList<- getPtDefs()$pts
    tagRList<-getPtDefs()$df
    pts<-     getPtDefs()$pts[[name]] 
    df<-      tagRList[[name]]
    tags<-    df$tag
    ti<-which(index==tags) 
    id.nos<-sequence(ncol(pts))
    tagInterval<-findInterval(id.nos,tags)
    ptsA<-pts[,tagInterval<ti]
    ptsB<-pts[,tagInterval>ti]
    
    tiSize<-ncol(pts[,tagInterval==ti])
    
    ptsNew<-matrix(c(ptsA,ptsB),2)
    t2<-tags[tags>index]-tiSize
    t1<-tags[tags<index]
    tagsNew<-c(t1,t2)
    
    df1<-subset(df,df$tag<index)
    df2<-subset(df,df$tag>index)
    dfNew<-as.data.frame(rbind(df1,df2))
    dfNew$tag<-tagsNew
    ptRList[[name]]<-ptsNew
    tagRList[[name]]<-dfNew
    scr<-getCode()
    src<-user$code
    src<-pts2Source(src,ptRList)
    src<-df2Source( src, tagRList)
    
    #will need to handle case when no more tagged points!!!
    #update
    user$code<-src
    selectedPoint$point.index<-as.numeric(index)-tiSize
  }
)

#swapTagPoints<-function(pts, df, t0,t1){
#
#    tags<-    df$tag
#    t0<-which(index==tags) 
#    t1<-t0+1
#    
#    id.nos<-sequence(ncol(pts))
#    tagInterval<-findInterval(id.nos,tags)
#    ptsA   <-pts[,tagInterval<t0]
#    ptsB   <-pts[,tagInterval>t1]
#    ptsT0  <-pts[,tagInterval==t0]
#    ptsT1  <-pts[,tagInterval==t1]
#    pts <-matrix(c(ptsA,ptsT1, ptsT0, ptsB),2)
#    
#    df[c(t0,t1)]<-df[c(t1,t0)]
#    t1Size      <-ncol(ptsT1)
#    tags[t1]    <-tags[t0]+t1Size
#    df$tag      <-tags
#
#}

observeEvent( 
  tagDragInfoList$tagMoveUp(),
  {
    name<-    tagDragInfoList$name()
    index<-   tagDragInfoList$index()
    ptRList<- getPtDefs()$pts
    tagRList<-getPtDefs()$df
    
    pts<-     ptRList[[name]] 
    df<-      tagRList[[name]]
    
    tags<-    df$tag
    t0<-which(index==tags) 
    t1<-t0+1
    
    
    id.nos<-sequence(ncol(pts))
    tagInterval<-findInterval(id.nos,tags)
    ptsA   <-pts[,tagInterval<t0]
    ptsB   <-pts[,tagInterval>t1]
    ptsT0  <-pts[,tagInterval==t0]
    ptsT1  <-pts[,tagInterval==t1]
    pts <-matrix(c(ptsA,ptsT1, ptsT0, ptsB),2)
    
    df[c(t0,t1),]<-df[c(t1,t0),]
    t1Size      <-ncol(ptsT1)
    tags[t1]    <-tags[t0]+t1Size
    df$tag      <-tags
    
    ptRList[[name]]  <-pts
    tagRList[[name]] <-df
    
    scr<-getCode()
    src<-user$code
    src<-pts2Source(src,ptRList)
    src<-df2Source( src, tagRList)
    
    #will need to handle case when no more tagged points!!!
    #update
    user$code<-src
    selectedPoint$point.index<-tags[t1]
  }
)

observeEvent( 
  tagDragInfoList$tagMoveDown(),
  {
    name<-    tagDragInfoList$name()
    index<-   tagDragInfoList$index()
    ptRList<- getPtDefs()$pts
    tagRList<-getPtDefs()$df
    
    pts<-     ptRList[[name]] 
    df<-      tagRList[[name]]
    
    tags<-    df$tag
    t1<-which(index==tags) 
    t0<-t1-1
    
    
    id.nos<-sequence(ncol(pts))
    tagInterval<-findInterval(id.nos,tags)
    ptsA   <-pts[,tagInterval<t0]
    ptsB   <-pts[,tagInterval>t1]
    ptsT0  <-pts[,tagInterval==t0]
    ptsT1  <-pts[,tagInterval==t1]
    pts <-matrix(c(ptsA,ptsT1, ptsT0, ptsB),2)
    
    df[c(t0,t1),]<-df[c(t1,t0),]
    t1Size      <-ncol(ptsT1)
    tags[t1]    <-tags[t0]+t1Size
    df$tag      <-tags
    
    ptRList[[name]]  <-pts
    tagRList[[name]] <-df
    
    scr<-getCode()
    src<-user$code
    src<-pts2Source(src,ptRList)
    src<-df2Source( src, tagRList)
    
    #will need to handle case when no more tagged points!!!
    #update
    user$code<-src
    selectedPoint$point.index<-tags[t0]
  }
)

#-------------------------------------------


  showPts.dragTag %<c-% function(ptName, pts, selectedPointIndx, 
  ptDisplayMode,  tags=NULL){
    #cat(file=stderr(),"entering drag.Tag\n")
    #print("entering drag.Tag")
    if( is.null(pts) ) {return(NULL) } 
    if(length(pts)<2)  {return(NULL) }
    tag.indx<-selectedPointIndx #this is the position of the first point of the tagged set 
    semitransparent<-0.3
    colorScheme<-c(default="purple", ending="red", selected="blue")
    color<-colorScheme[1]
    m<-matrix(pts,2)
    if( !is.null(tag.indx) && !is.null(tags)){
      ti<-which(max(tags[tags<=tag.indx])==tags )
      id.nos<-sequence(ncol(m))
      ids<-paste("pd",ptName,id.nos,sep="-")
      tagInterval<-findInterval(id.nos,tags)
      tagIntList<-tapply(id.nos, findInterval(id.nos,tags), list )
      opacity<-rep(semitransparent, length(tags))
      opacity[ti]<-1
      # iterate over tagIntList
      indx<-unique(tagInterval)
      indx<-indx[-ti]
      list( 
        lapply(indx, function(i){
          g( opacity=opacity[i], 
             fill='purple',
             transform="matrix(1 0 0 1 0 0)", 
             #onmousedown="selectElement(evt)",
             tid=paste0("ptR_Tag_",i),
             lapply(tagIntList[[i]], function(j){
               circle(   cxy=m[,j], r=8)
             })
          )
        }),
        g( opacity=opacity[ti], 
           fill='purple',
           transform="matrix(1 0 0 1 0 0)", 
           onmousedown="selectElement(evt)",
           tid=paste0("ptR_Tag_",ti),
           lapply(tagIntList[[ti]], function(j){
             circle(   cxy=m[,j], r=8)
           })
        )
      )
      } #end if
  } #end showPts




####################
#
#output$svgTagDragPanel<-renderUI({
#  conditionalPanel( "input.plotNavBar=='tagDrag'", 
#    absolutePanel( top=130, left=0, right=0,  draggable=FALSE,
#                     style=cstyle$svg, htmlOutput("svgDragTagPlot")
#      )
#  )
#})
#  
#
#output$svgDragTagPlot <- renderUI({
#  WH<-getSVGWH()
#  showGrid<-showGrid()
#  if(is.null(showGrid)){return(NULL)}
# 
#  ptName<-getPtName()
#  ptRList<-getPtDefs()$pts
#  ptDisplayMode<-displayMode()
#  src<-getCode()  
#  src<-usingDraggable(src)
#  
#  
#  ptrDisplayScript<-js.scripts[[ "transTag"]]
#  
#  
#  showPts<-function(ptName, ptRList, ptDisplayMode){
#    if(!is.null(ptName) && !is.null(ptRList)){
#      pts<- ptRList[[ptName]]
#      selectedPointIndx<-as.numeric( getPtIndex() )
#    } else {
#      pts<-NULL
#      selectedPointIndx<-0
#    }
#    
#    tagRList<-getPtDefs()$df 
#    if(!is.null(tagRList)){
#      tags<-tagRList[[ptName]]$tag
#    } else {
#      tags<-NULL
#    }
#    ptDisplayMode<-"normal"
#    showPts.dragTag(ptName, pts=pts, 
#      selectedPointIndx=selectedPointIndx, 
#      ptDisplayMode=ptDisplayMode,  tags=tags
#    )
#  }
#  
#  #defining the prolog 
#    insert.beg<-c( 
#      'style(".draggable {','cursor: move;','}"),', 
#      gsub('ptrDisplayScript', ptrDisplayScript, "script('ptrDisplayScript'),"),      
#      "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),",
#      if(showGrid()==TRUE){"graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),"} 
#      else { NULL }
#    )
#  
#  #defining the epilog
#  insert.end<-c(
#    ',showPts(ptName, ptRList,  ptDisplayMode)'
#  )    
#  
#  #put it together
#  src<-subSVGX2(src, insert.beg, insert.end)
#  res<-""
#    tryCatch({
#        parsedCode<-parse(text=src)
#        svg<-eval(parsedCode)
#        as.character(svg)->svgOut 
#        res<-HTML(svgOut)
#        backup$code<-getCode()  
#        backup$res<-res
#      },
#      error=function(e){
#        # session$sendCustomMessage(type='testmessage', message=e)
#        mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
#        # cat(mssg$error)
#        user$code<-getCodeBackUp()
#        updateNavbarPage(session, "plotNavBar", selected ="Log")
#      } 
#    )
#  res
#})
#


output$svgTagDragPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='tagDrag'", modulePlotSVGrUI("svgTagDragMod"))
})

tagValSVGList<-callModule(
  module=modulePlotSVGr,
  id="svgTagDragMod",
  svgID='ptR_SVG_TagDrag',
  showPts.compound=reactive({
    print(getTagName())
    print(getPtDefs()$pts[[getTagName()]])
    print(getTagIndex() )
    showPts.dragTag(
      ptName=getTagName(), pts=getPtDefs()$pts[[getTagName()]],
      selectedPointIndx=as.numeric( getTagIndex() ),
      ptDisplayMode=getDisplayModeTag(), tags=getTagDF()$tag 
    )
  }),
  ptrDisplayScript = reactive({ js.scripts[[ "TagDrag"]] }),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getCodeBackup,
  getErrorMssg,
  insert.end=",showPts.compound()"
)
