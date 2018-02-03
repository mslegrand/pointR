
# showGrid<-reactive({displayOptions$showGrid})
# 
# 
# ptrDisplayScript =reactive({ 
#   type=getRightMidPanel2()
#   cat("\n-------------type1=",type,"\n")
#   if(type=='transform'){
#     type=  paste0(type,".",getTransformType() )
#     cat("\n-------------type2=",type,"\n")
#   }
#   
#   scripts<-list(
#     point=    'var ptRPlotter_ptR_SVG_Point = new PtRPanelPoints("ptR_SVG_Point");',
#     value=    'var ptRPlotter_ptR_SVG_TagVal = new PtRPanelTagVal("ptR_SVG_TagVal");',
#     transform.Translate= 'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");',
#     transform.Rotate=    'var ptRPlotter_ptR_SVG_TRANSFORM_ROTATE = new PtRPanelRotate("ptR_SVG_TRANSFORM");',
#     transform.Scale=     'var ptRPlotter_ptR_SVG_TRANSFORM_SCALE = new PtRPanelScale("ptR_SVG_TRANSFORM");',
#     matrix=    'var ptRPlotter_ptR_SVG_TagDrag = new PtRPanelTagDrag("ptR_SVG_TagDrag");'
#   )
#   scripts[type]
# })

displayOptions<-reactiveValues(
  insertMode=TRUE,
  showGrid=FALSE,
  ptMode="Normal" # can be 'Hidden', 'Normal', 'Labeled'
)
displayMode<-reactive({displayOptions$ptMode})

setDisplayOption<-function( insertMode, showGrid, ptMode){
  if(!missing(insertMode)){
    displayOptions$insertMode<-insertMode
  }
  if(!missing(showGrid)){
    displayOptions$showGrid<-showGrid
  }
  if(!missing(ptMode)){
    displayOptions$ptMode<-ptMode
  }
}

#this is tagDisplay Mode
getDisplayModeTag<-reactive({
    displayMode()
})

getDisplayMode<-reactive({
  cat("displayOptions$ptMode=",displayOptions$ptMode,"\n")
  displayOptions$ptMode
})


getInsertMode<-reactive({displayOptions$insertMode })