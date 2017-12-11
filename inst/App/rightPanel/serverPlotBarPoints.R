
# --------------input$plotNavBar=="Points"----------------


# ===============Begin Server PointsBar=======================



#CALL modulePointsBarUI

returnValue4ModulePointsBar<-callModule( #auto  input, output, session 
  module=modulePointsBar, 
  id="pointsBar", 
  barName=rightPanel,
  name=getTibName, 
  nameChoices=getTibNameChoices,
  rowIndex=getTibRow,
  rowIndexChoices=getTibRowChoices,
  matColIndex=getTibMatCol,
  matColIndexChoices=getTibMatColChoices,
  headerId=NS("pointsBar", 'header')
)

#-----REACTIVES   based on modulePointsBar::returnValue4ModulePointsBar

observeEvent(returnValue4ModulePointsBar$showGrid() ,{
  if(rightPanel()=="Points"){
    displayOptions$showGrid<-returnValue4ModulePointsBar$showGrid() 
  }
})



displayMode<-reactive({returnValue4ModulePointsBar$displayMode()})
insertMode<-reactive({returnValue4ModulePointsBar$insertMode() })

#-----OBSERVERS  using  modulePointsBar::returnValue4ModulePointsBar

# --SELECTION EVENTS-------------------------------
observeEvent(returnValue4ModulePointsBar$name(), {
  if(rightPanel()=="Points"){
    name<-returnValue4ModulePointsBar$name()
    if(!is.null(name)){
      updateSelected(name=returnValue4ModulePointsBar$name())
    }
  }
})



#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( returnValue4ModulePointsBar$removePt(), {
  selection<-getTibName() 
cat('Enter removePt\n')  
  if(selection!=""){
    ptDefs<-getPtDefs()
    if(length(ptDefs$tib)==0){return(NULL)}
    matCol<-getTibMatCol()
    #src<-getCode() 
    
    #get row, col
    if(matCol>=1){ 
      row<-getTibRow()
      m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]][,-matCol] 
      #!!! probably need some checking here
      ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m
      matCol<-min(matCol, length(m)/2)
      newPtDefs<-ptDefs
      sender='points.deletePoint'
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(matCol=matCol)
    }
  }
}) #end remove point observer

#----begin for Tagging-------------------------------------

# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
modalFreq <- function(failed = FALSE) {
  doOk<-'shinyjs.triggerButtonOnEnter(event,"okTag")'
  modalDialog(
    onkeydown=doOk,
    selectInput("tagFreq", "Auto Tag",
                c(list("Off"),1:20), selected="Off", 
                multiple=FALSE, selectize = FALSE,
                width="80px", size=1  ), 
    span('Start tagging current point matrix'), 
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okTag", "OK")
    )
  ) 
}



#---TAG THIS POINT button-----
# note: in 1st tag, calls freqModal to complete the work, which exits in the okTag above
observeEvent( returnValue4ModulePointsBar$tagPt(), {
  
  if(rightPanel()=="Points"){
    #selection<-input$ptRSelect
    cat("Enter tagPt\n")
    src<-getCode() 
    selection<-getTibName()
    ptDefs<-getPtDefs()
    
    rc<-list( row=getTibRow(), matCol=getTibMatCol() )
    
    m<-ptDefs$tib[[selection]][[ rc$row, getTibPtColPos() ]]
    if(ncol(m)<1){ 
      return(NULL) # bail if matrix of points is empty
    }
    tib<-ptDefs$tib[[selection]] #get the tib 
    tib<-tagTib(tib, getTibPtColPos(), rc$row, rc$matCol)
    
    ptDefs$tib[[selection]]<-tib 
    sender='tagPt'
    updateAceExtDef(ptDefs, sender=sender)
    updateSelected(row=getTibRow()+1, matCol=1)
  } #end of if
}) #end of point InfoList Tag Point, 


observeEvent( returnValue4ModulePointsBar$forwardPt(), {
  matColIndex<-getTibMatCol()
  if(length( matColIndex)>0){
    cat("observeEvent:: serverPlotBar 99\n")
    matColIndex=max(matColIndex+1, min(getTibMatColChoices()) )
    updateSelected(  matCol=matColIndex )
  }
})

observeEvent( returnValue4ModulePointsBar$backwardPt(), {
  matColIndex<-getTibMatCol()
  if(length(matColIndex)>0){
    cat("observeEvent:: serverPlotBar 98\n")
    matColIndex=max(matColIndex-1, min(getTibMatColChoices()) )
    updateSelected(  matCol=matColIndex  )
  }
})

# ===============END SERVER Module PointsBar=======================



# ===============BEGIN SERVER Module svgPointsMod=======================

#---------ShowPts----------------------------------

  showPts.PtCmd %<c-% function(
      ptName, 
      pts=NULL,  
      rowIndex=NULL,
      matColIndex=NULL,
      ptDisplayMode="Normal"
  ){
    onMouseDownTxt='ptRPlotter_ptR_SVG_Point.selectPoint(evt)'
    
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    
    # cat("pts-----------------------------------------------\n")
    # print(pts)
    if(length(unlist(pts))<2){ return(NULL)}
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    semitransparent<-0.3
    
    cat("rowIndex",rowIndex,"\n")
    cat("class(rowIndex)",class(rowIndex),"\n")
    
    cat("matColIndex",matColIndex,"\n")
    cat("class(matColIndex)",class(matColIndex),"\n")
    
    opacity<-rep(semitransparent, length(pts) )
    opacity[rowIndex]<-1
    
    #form list of  all point renderings
    lapply(seq(length(pts)), function(i){
      m<-pts[[i]]
      if(length(m)==0){ # or !is(m,'matrix')
        NULL
      } else {
        lapply(seq(ncol(m)), function(j){ #j is the matCol index
          
          id<-paste("pd",ptName,i,j,sep="-")
          cat("id=", id, "\n")
          pt<-m[,j]
          color=colorScheme['default']
          
          list(
            if(i==rowIndex && j== matColIndex ){
              circle(class="draggable", 
                     id=id,  
                     cxy=pt, r=9, fill="yellow", 
                     opacity=opacity[i],
                     stroke=colorScheme['selected'], stroke.width=3,
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
              )
            } else { #a non-selected point
              circle(class="draggable", 
                     id=id,  
                     cxy=pt, r=8, fill=color, opacity=opacity[i],
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
              )
            },
            if(ptDisplayMode=="Labeled"){
              text(paste0(i,",",j), cxy=pt+10*c(1,-1),  
                   stroke='black', font.size=12, opacity=1) 
            } else {
              NULL
            }
          )
        }) #end lapply of this row
      }
 
    }) #end lapply of points
  } #end showPts.PtCmd





#===============


# !!! todo: recode onmousedown.newPt
newPtLayer %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    onmousedownNewPt="ptRPlotter_ptR_SVG_Point.newPoint(evt)"
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', 
         opacity=.0, onmousedown=onmousedownNewPt)
           #"ptRPlotter_ptR_SVG_Point.newPoint(evt)")
  } else {
    NULL
  } 
}
  
#===============
  
statusPlotPoint<-callModule(
  module=modulePlotSVGr,
  id="svgPointsMod",
  svgID='ptR_SVG_Point',
  showPts.compound=reactive({
    list(
      newPtLayer( insertMode(), getSVGWH() ),
      showPts.PtCmd(
        ptName=getTibName(), 
        pts=getTibPts(), #getPtDefs()$pts[[getPtName()]],
        rowIndex=getTibRow(),
        matColIndex=getTibMatCol(),
        ptDisplayMode=displayMode()
      )
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "Points") }), #ptrDisplayScript =reactive({ js.scripts[[ "Points"]] }),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",showPts.compound()"
)

#error handler
observeEvent(statusPlotPoint$status(), {
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){ 
    updateRightPanel('logPanel')
    mssg$err<-status$message
  }
})



   