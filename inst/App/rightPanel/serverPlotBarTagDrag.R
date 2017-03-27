
# --------------input$plotNavBar=="dragTag"---------------- 

 # output$TagDragPanel<-renderUI({
 #  conditionalPanel( "input.plotNavBar=='tagDrag'", moduleTagDragUI("tagDragBar"))
 # })

tagDragInfoList<-callModule(
  module=moduleTagDrag,
  id="tagDragBar",
  barName=rightPanel,
  getTagNameChoices=getTagNameChoices,
  getTagName=getTagName,
  getTagIndexChoices=getTagIndexChoices,
  getTagIndex=getTagIndex
)

observe({
  name<-tagDragInfoList$name()
  index<-tagDragInfoList$index()
  if(rightPanel()=="tagDrag"){
    isolate({
      if(!is.null(name)){
        selectedPoint$name<-name
        selectedPoint$point.index<-as.numeric(index)
      }
    })  
  }
})

observeEvent( 
  tagDragInfoList$tagClone(),
  {
    if(rightPanel()=="tagDrag"){
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
      
      tiSize<-ncol(matrix(pts[,tagInterval==ti],2))
      
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
  }
)

#delete tag set
observeEvent( 
  tagDragInfoList$tagDelete(),
  {
    if(rightPanel()=="tagDrag"){
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
    if(rightPanel()=="tagDrag"){
      name<-    tagDragInfoList$name()
      index<-   tagDragInfoList$index()
      if(is.null(index)||index==0){ return(NULL) }
      ptRList<- getPtDefs()$pts
      tagRList<-getPtDefs()$df
      
      pts<-     ptRList[[name]] 
      df<-      tagRList[[name]]
      
      tags<-    df$tag
      t0<-which(index==tags) 
      t1<-t0+1
      if(t1>length(tags)){return(NULL)}
      
      id.nos<-sequence(ncol(pts))
      tagInterval<-findInterval(id.nos,tags)
      ptsA   <-pts[,tagInterval<t0]
      ptsB   <-pts[,tagInterval>t1]
      ptsT0  <-pts[,tagInterval==t0]
      ptsT1  <-pts[,tagInterval==t1]
      pts <-matrix(c(ptsA,ptsT1, ptsT0, ptsB),2)
      
      df[c(t0,t1),]<-df[c(t1,t0),]
      t1Size      <-ncol(matrix(ptsT1,2))
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
  }
)

observeEvent( 
  tagDragInfoList$tagMoveDown(),
  {
    if(rightPanel()=="tagDrag"){
      name<-    tagDragInfoList$name()
      index<-   tagDragInfoList$index()
      if(is.null(index)||index==0){ return(NULL) }
      ptRList<- getPtDefs()$pts
      tagRList<-getPtDefs()$df
      
      pts<-     ptRList[[name]] 
      df<-      tagRList[[name]]
      
      tags<-    df$tag
      t1<-which(index==tags) 
      t0<-t1-1
      if(t0==0){ return(NULL) }
      
      
      id.nos<-sequence(ncol(pts))
      tagInterval<-findInterval(id.nos,tags)
      ptsA   <-pts[,tagInterval<t0]
      ptsB   <-pts[,tagInterval>t1]
      ptsT0  <-pts[,tagInterval==t0]
      ptsT1  <-pts[,tagInterval==t1]
      pts <-matrix(c(ptsA,ptsT1, ptsT0, ptsB),2)
      
      df[c(t0,t1),]<-df[c(t1,t0),]
      t1Size      <-ncol(matrix(ptsT1,2))
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
  }
)

#-------------------------------------------


  showPts.dragTag %<c-% function(ptName, pts, 
  selectedPointIndx, ptDisplayMode,  tags=NULL){
    if(length(ptName)<1){return(NULL)}
    if(length(pts)<2)  {return(NULL) }
    if(length(tags)<1){return(NULL)}
    if(length(selectedPointIndx)<1 || selectedPointIndx==0){return(NULL)}

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
               list(
                  circle(   cxy=m[,j], r=8),
                  if(ptDisplayMode=="Labeled"){
                    text(paste(j), cxy=m[,j]+10*c(1,-1),  stroke='black', font.size=12) #opac)
                  } else {
                    NULL
                  }
               )
             })
          )
        }),
        g( opacity=opacity[ti], 
           fill='purple',
           transform="matrix(1 0 0 1 0 0)", 
           onmousedown="selectElement(evt)",
           tid=paste0("ptR_Tag_",ti),
           lapply(tagIntList[[ti]], function(j){
            list(
             circle(   cxy=m[,j], r=8),
             if(ptDisplayMode=="Labeled"){
                  text(paste(j), cxy=m[,j]+10*c(1,-1),  stroke='black', font.size=12) #opac)
                } else {
                  NULL
                }
            )
           })
        )
      )
      } #end if
  } #end showPts


#-------------SVG-------------------------------------
# 
# output$svgTagDragPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar=='tagDrag'", modulePlotSVGrUI("svgTagDragMod"))
# })

tagValSVGList<-callModule(
  module=modulePlotSVGr,
  id="svgTagDragMod",
  svgID='ptR_SVG_TagDrag',
  showPts.compound=reactive({
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
