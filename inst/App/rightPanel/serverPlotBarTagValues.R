

# --------------input$plotNavBar=="tagValues"---------------- 
 # output$TagValuesPanel<-renderUI({
 #  conditionalPanel( "input.plotNavBar=='tagValues'", moduleTagValUI("tagValBar"))
 # })

returnValue4ModuleTagVal<-callModule(
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

#observes returnValue4ModuleTagVal$name, returnValue4ModuleTagVal$index
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$index()),{
  if(rightPanel()=='tagValues'){
    name<-returnValue4ModuleTagVal$name()
    index<-returnValue4ModuleTagVal$index()
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
  }
})


observeEvent(returnValue4ModuleTagVal$updateTagsNow(),{
  if(returnValue4ModuleTagVal$updateTagsNow()>0 
     && rightPanel()=='tagValues'){
    name<-returnValue4ModuleTagVal$name()
    index<-returnValue4ModuleTagVal$index()
    if(!is.null(name)){
      selectedPoint$name<-name
      selectedPoint$point.index<-as.numeric(index)
    }
    tagRList<-returnValue4ModuleTagVal$tagRList()
    if( !is.null(tagRList) ){
      newPtDefs<-getPtDefs()
      newPtDefs$df<-tagRList
      updateAceExtDef(newPtDefs, sender="tagVal.UpdateTagsNow")
      #src<-df2Source(getCode(),tagRList)
      # setCode(src) #!!!
    }
  }
})


#----------------------------------------------------------------
#------------SVG DISPLAY--------------------------------------------
#----------------------------------------------------------------


showPts.valTag %<c-% function(ptName, pts, 
                              selectedPointIndx, ptDisplayMode,  tags=NULL){
  
  if(length(ptName)<1){return(NULL)}
  if(length(pts)<2)  {return(NULL) }
  if(length(tags)<1){return(NULL)}
  if(length(selectedPointIndx)<1 || selectedPointIndx==0){return(NULL)}
  
  tag.indx<-selectedPointIndx #this is the position of the first point of the tagged set 
  semitransparent<-0.3
  colorScheme<-c(default="green", ending="red", selected="blue")
  color<-colorScheme[1]
  m<-matrix(pts,2)
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
  tidPrefix<-"ptR_TagV_"
  
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
           onmousedown=onMouseDownTxt,
           tid=paste0(tidPrefix,i),
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
         onmousedown=onMouseDownTxt,
         tid=paste0(tidPrefix,ti),
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


statusPlotTagVal<-callModule(
  module=modulePlotSVGr,
  id="svgTagValsMod",
  svgID='ptR_SVG_TagVal',
  showPts.compound=reactive({
    showPts.valTag(
      ptName=getTagName(), 
      pts=matrix(unlist(getPtDefs()$tib[[getTagName()]]),2) ,
      selectedPointIndx=as.numeric( getTagIndex() ),
      ptDisplayMode=getDisplayModeTag(), 
      tags=getTagIndexChoices()
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagVal") }), #ptrDisplayScript = reactive({ js.scripts[[ "TagVal"]]}),
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

observeEvent(statusPlotTagVal$status(), {
  status<-statusPlotTagVal$status()
  if(status$state!="PASS"){
    updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    # switch to log 
  }
})





