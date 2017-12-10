

# --------------input$plotNavBar=="tagValues"---------------- 
 # output$TagValuesPanel<-renderUI({
 #  conditionalPanel( "input.plotNavBar=='tagValues'", moduleTagValUI("tagValBar"))
 # })

returnValue4ModuleTagVal<-callModule(
  module=moduleTagVal,
  id="tagValBar",
  id2="tagValBar", # !!! DO  WE STILL NEED THIS???? 
  barName=rightPanel ,
  name=getTibName,
  nameChoices=getTibNameChoices,
  ptIndex=getPtIndex,
  ptIndexChoices=getPtIndexChoices,
  rowIndex=getTibRow,
  rowIndexChoices=getTibRowChoices,
  matColIndex=getTibMatCol,
  matColIndexChoices=getTibMatColChoices,
  columnName= getTibColumnName,
  columnNameChoices=getTibColumnNameChoices
  # getCode=getCode, # or wrap
  # getPtDefs=reactive({x<-getPtDefs(); x}),
  # getTagNameChoices=reactive({getTagNameChoices()}) ,
  # getTagName=reactive({getTagName()}),
  # getTagIndexChoices=reactive({getTagIndexChoices()}),
  # getTagIndex=reactive({getTagIndex()})
)

#observes returnValue4ModuleTagVal$name, returnValue4ModuleTagVal$index
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$rowIndex()),{
  if(rightPanel()=='tagValues'){
    name<-returnValue4ModuleTagVal$name()
    rowIndex<-returnValue4ModuleTagVal$rowIndex()
    if(!is.null(name)){
      newTib<-getPtDefs()$tib[[name]]
      matColIndex<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      pts<-newTib[[getTibPtColPos()]]
      point.index<-ptPos2AbsPtIndx(pts, rowIndex, matColIndex)
      # selectedPoint$name<-name
      # selectedPoint$point.index<-as.numeric(index)
      #point.index<-as.numeric(index)
      #rc<-absPtIndx2TibPtPos(point.index)
      
      updateSelected(name=name, row=rowIndex, 
                     matCol=matColIndex, 
                     point.index=point.index)
    }
  }
})




#----------------------------------------------------------------
#------------SVG DISPLAY--------------------------------------------
#----------------------------------------------------------------


showPts.valTag %<c-% function(
  ptName=NULL, 
  pts=NULL, 
  #selectedPointIndx, 
  rowIndex=NULL,
  ptDisplayMode #,  
  #tags=NULL
  ){
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
 
  #cat("rowIndx=", rowIndex, "\n")
  if(length(ptName)<1){return(NULL)}
  if(length(pts)<2)  {return(NULL) }
  
  if(length(rowIndex)<1 || rowIndex==0){return(NULL)}
  
  #tag.indx<-selectedPointIndx #this is the position of the first point of the tagged set 
  semitransparent<-0.3
  colorScheme<-c(default="purple", ending="red", selected="blue")
  color<-colorScheme[1]
  #m<-matrix(pts,2)
  
  
  
  opacity<-rep(semitransparent, length(pts)) 
  opacity[rowIndex]<-1 
  rowNums<-seq(length(pts))
  ids<-paste("pd",ptName,rowNums,sep="-")
  offRows<-rowNums[-rowIndex]
  mRow<-pts[[rowIndex]]
  
  list( 
    lapply(offRows, function(i){
      m<-pts[[i]]
      g( opacity=opacity[i], 
         fill='purple',
         transform="matrix(1 0 0 1 0 0)", 
         onmousedown=onMouseDownTxt,
         tid=paste0("ptR_Tag_",i),
         lapply(seq(ncol(m)), function(j){
           list(
             circle(cxy=m[,j], r=8),
             if(ptDisplayMode=="Labeled"){
               text( paste(j), cxy=m[,j]+10*c(1,-1),  stroke='black', font.size=12) 
             } else {
               NULL
             }
           )
         })
      )
    }),
    g( opacity=opacity[rowIndex], 
       fill='purple',
       transform="matrix(1 0 0 1 0 0)", 
       onmousedown=onMouseDownTxt,
       tid=paste0("ptR_Tag_",rowIndex),
       lapply(seq(ncol(mRow)), function(j){
         list(
           circle(   cxy=mRow[,j], r=8),
           if(ptDisplayMode=="Labeled"){
             text(paste(j), cxy=mRow[,j]+10*c(1,-1),  stroke='black', font.size=12) #opac)
           } else {
             NULL
           }
         )
       })
    )
  ) #end list
} #end showPts



statusPlotTagVal<-callModule(
  module=modulePlotSVGr,
  id="svgTagValsMod",
  svgID='ptR_SVG_TagVal',
  showPts.compound=reactive({
    showPts.valTag(
      ptName=getTibName(), 
      pts=getTibPts(), #matrix(unlist(getPtDefs()$tib[[getTagName()]]),2) ,
      #selectedPointIndx=as.numeric( getTagIndex() ),
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayModeTag() #, 
      #tags=getTagIndexChoices()
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





