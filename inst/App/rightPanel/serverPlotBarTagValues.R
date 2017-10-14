

# --------------input$plotNavBar=="tagValues"---------------- 
 # output$TagValuesPanel<-renderUI({
 #  conditionalPanel( "input.plotNavBar=='tagValues'", moduleTagValUI("tagValBar"))
 # })

tagValInfoList<-callModule(
  module=moduleTagVal,
  id="tagValBar",
  id2="tagValBar",
  barName=rightPanel ,
  getCode=getCode, # or wrap
  getPtDefs=reactive({x<-getPtDefs(); x}),
  getTagNameChoices=reactive({getTagNameChoices()}) ,
  getTagName=reactive({getTagName()}),
  getTagIndexChoices=reactive({getTagIndexChoices()}),
  getTagIndex=reactive({getTagIndex()})
)

#observes tagValInfoList$name, tagValInfoList$index
observeEvent(c(tagValInfoList$name(),tagValInfoList$index()),{
  if(rightPanel()=='tagValues'){
    name<-tagValInfoList$name()
    index<-tagValInfoList$index()
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
  }
})


observeEvent(tagValInfoList$updateTagsNow(),{
  if(tagValInfoList$updateTagsNow()>0 
     && rightPanel()=='tagValues'){
    name<-tagValInfoList$name()
    index<-tagValInfoList$index()
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
    tagRList<-tagValInfoList$tagRList()
    if( !is.null(tagRList) ){
      newPtDefs<-getPtDefs()
      newPtDefs$df<-tagRList
      updateAceExtDef(newPtDefs, "tagVal.UpdateTagsNow")
      #src<-df2Source(getCode(),tagRList)
      # setCode(src) #!!!
    }
  }
})


#----------------------------------------------------------------
#------------SVG DISPLAY--------------------------------------------
#----------------------------------------------------------------

showPts.valTag%<c-% function(ptName, pts, selectedPointIndx, ptDisplayMode,  ptTags){
    
    if(length(ptName)<1){return(NULL)}
    if(length(pts)<2){return(NULL)}
    if(length(ptTags)<1){return(NULL)}
    if(selectedPointIndx==0){return(NULL)}
    
    tag.indx<-selectedPointIndx
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


statusPlotTagVal<-callModule(
  module=modulePlotSVGr,
  id="svgTagValsMod",
  svgID='ptR_SVG_TagVal',
  showPts.compound=reactive({
    showPts.valTag(
      ptName=getTagName(), pts=getPtDefs()$pts[[getTagName()]],
      selectedPointIndx=as.numeric( getTagIndex() ),
      ptDisplayMode=getDisplayModeTag(), ptTags=getTagDF() 
    )
  }),
  ptrDisplayScript = reactive({ js.scripts[[ "TagVal"]]}),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",showPts.compound()"
)

observeEvent(statusPlotTagVal$status(), {
  status<-statusPlotTagVal$status()
  if(status$state!="PASS"){
    updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    # switch to log 
  }
})




