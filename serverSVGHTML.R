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
   
  ptName<-selectedPoint$name
  ptRList<-getPtDefs()$pt
  ptDisplayMode<-displayOptions$ptMode
    
  showGrid<-displayOptions$showGrid
  if(is.null(showGrid)){
    return(NULL)
  }
  
  ptrDisplayScript<-switch(svgBarCmd,
    Points     =js.scripts[[ "Points"]],
    tagValues  =js.scripts[[ "Points"]],
    dragTag    =js.scripts[[ "transTag"]],
    Transforms = js.scripts[[ input$transformOption ]]
  )
  
  src<-user$code
  
  if(svgBarCmd=="Transforms"){
    src<-usingDraggable(src)
  }
  
  showPts.transform %<c-% function(){ NULL }
  
# called when we need to show points
# to do: rewrite to make this work with call for tags
# where each tag is a group, so that we can edit a tag set 
# to provide ability for translate, rotate, scale of points

# dragtag, magtag, wagtag, zagtag ? bagtag?
  
  showPts<-function(ptName, ptRList, ptDisplayMode){
    if(!is.null(ptName) && !is.null(ptRList)){
      pts<- ptRList[[ptName]]
      selectedPointIndx<-as.numeric( selectedPoint$point.index )
    } else {
      pts<-NULL
      selectedPointIndx<-0
    }
    switch(svgBarCmd,
      Points     = {  
        showPts.PtCmd(ptName, pts=pts,  selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode)
      },
      tagValues    = { 
        tagRList<-getPtDefs()$df 
        if(!is.null(tagList)){
          ptTags<-tagRList[[ptName]]
        } else {
          ptTags<-NULL
        }
        if(ptDisplayMode=="hidden"){ptDisplayMode<-"normal"}
        showPts.valTag(ptName, pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  ptTags=ptTags)
      },      
      dragTag    = { 
        tagRList<-getPtDefs()$df 
        if(!is.null(tagRList)){
          tags<-tagRList[[ptName]]$tag
        } else {
          tags<-NULL
        }
        ptDisplayMode<-"normal"
        showPts.dragTag(ptName, pts=pts, selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode,  tags=tags)
      },
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
    "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),",
    if(displayOptions$showGrid==TRUE){"graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),"} 
    else { NULL }
  )
  
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
