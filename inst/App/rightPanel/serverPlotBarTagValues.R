

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
  columnNameChoices=getTibColumnNameChoices,
  getTibEntry=getTibEntry,
  getTibEntryChoices=getTibEntryChoices
)

#name, rowIndex
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$rowIndex()),{
  if(rightPanel()=='tagValues'){
    name<-returnValue4ModuleTagVal$name()
    rowIndex<-returnValue4ModuleTagVal$rowIndex()
    if(!is.null(name)){
      newTib<-getPtDefs()$tib[[name]]
      matColIndex<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      pts<-newTib[[getTibPtColPos()]]
      point.index<-ptPos2AbsPtIndx(pts, rowIndex, matColIndex)
    
      updateSelected(name=name, row=rowIndex, 
                     matCol=matColIndex, 
                     point.index=point.index)
    }
  }
})

#name, columnName
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$columnName()),{
  if(rightPanel()=='tagValues'){
    name<-returnValue4ModuleTagVal$name()
    colName<-returnValue4ModuleTagVal$columnName()
    #cat('colName==',colName,"\n")
    if(!is.null(name) ){
      columnNameChoices=getTibColumnNameChoices()
      ptPos<-getTibPtColPos()
      column<-match(colName, columnNameChoices, nomatch=ptPos)
      updateSelected(column=column)
    }
  }
})

# observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$rowIndex()),{
#   if(rightPanel()=='tagValues'){
#     name<-returnValue4ModuleTagVal$name()
#     rowIndex<-returnValue4ModuleTagVal$rowIndex()
#     if(!is.null(name)){
#       tib<-getPtDefs()$tib[[name]]
#       
#       matColIndex<-ncol(tib[[rowIndex, getTibPtColPos()]])
#       pts<-tib[[getTibPtColPos()]]
#       point.index<-ptPos2AbsPtIndx(pts, rowIndex, matColIndex)
#       updateSelected(name=name, row=rowIndex, 
#                      matCol=matColIndex, 
#                      point.index=point.index)
#     }
#   }
# })

#EACH OF THE FOLLOWING WILL REQUIRE A MODAL DIALOG
# 3 PARTS : 
#  1. FN to create modal instance (contains modalDialog in body) 
#    fn<-function(...){ modalDialog(xxxxx)}
#  2. call to showModal( fn(...))  (placed in the observeEvents below)
#  3. ok handler specific to the request to process that request and then close modal
#
# caveate: if not in module, must assure that ok event (input$okid) hasu unique
# to the app id., so intead of ok as the submit, we use "okDeleteTibCol" 

#--------DELETE COLUMN------------------------------

observeEvent(returnValue4ModuleTagVal$deleteColumn(),{
  if(rightPanel()=='tagValues'){
    # name<-returnValue4ModuleTagVal$name()
    # rowIndex<-returnValue4ModuleTagVal$rowIndex()
    # if(!is.null(name)){
    #   newTib<-getPtDefs()$tib[[name]]
    #   matColIndex<-ncol(newTib[[rowIndex, getTibPtColPos()]])
    #   pts<-newTib[[getTibPtColPos()]]
    #   point.index<-ptPos2AbsPtIndx(pts, rowIndex, matColIndex)
    #   
    #   updateSelected(name=name, row=rowIndex, 
    #                  matCol=matColIndex, 
    #                  point.index=point.index)
    # }
  }
})


#--------NEW COLUMN------------------------------
attrValueModal <- function(errMssg=NULL) {
  doOk<-"shinyjs.triggerButtonOnEnter(event,\"commitNewCol\")"
  modalDialog(
    onkeypress=doOk, 
    span('Enter both a name for the new column and a value for its entries'), 
    textInput("modalAttrName", "Enter the name for the new column"),
    textInput("modalAttrValue", "Enter an entry value for the new column"), 
    if(!is.null(errMssg)){
      div(tags$b(errMssg, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("commitNewCol", "Commit")
    )
  ) 
}

observeEvent(returnValue4ModuleTagVal$newColumn(),{
  if(identical( rightPanel(), 'tagValues')){
     showModal( attrValueModal() )
  }
})

observeEvent(input$commitNewCol, {
  if(identical( rightPanel(), 'tagValues')){
    #checks 
    if(!grepl(pattern = "^[[:alpha:]]", input$modalAttrName)){ # check name syntax
      showModal(attrValueModal( errMssg="Invalid Column Name: must begin with a character") )
    } else if( input$modalAttrName %in% names(getTib()) ){ # check name uniqueness
      showModal(attrValueModal( errMssg="Invalid Column Name: this name is already taken!") )
    } else if(!grepl(pattern = "^[[:graph:]]", input$modalAttrValue) ){  # check value uniqueness
      showModal(attrValueModal( errMssg="Invalid Column Value: must begin with printable character other than space") )
    } else { 
      #add name to tib
      newPtDefs<-getPtDefs()
      newColName<-input$modalAttrName
      
      newPtDefs$tib[[getTibName()]]<-add_column(newPtDefs$tib[[getTibName()]], 
                                                !!(newColName):=input$modalAttrValue   )     
      # updateAce
      
      sender<-'addNewColumn'
      updateAceExtDef(newPtDefs, sender=sender)
      # set selection to this column?
      removeModal() #close dialog
    }
  }
})



# modalEditValue<-function( oldValue, row, columnName){
#   #attrValueModal <- function(attrName, failedName=FALSE, failedValue=FALSE) {
#     #attrValueModal <- function(attrName, failedName=FALSE, failedValue=FALSE) {
#     doOk<-'updateTibVal'
#     modalDialog(
#       #onkeypress=doOk,
#       span(paste('row=',row),
#       span(paste('column=',columnName)
#       
#       textInput(ns("modalAttrValue"), "New  Value"), 
#       span('Enter new choice for the given named attribute'), 
#       if (failedName)
#         div(tags$b("Invalid Attribute Name: must begin with a character", style = "color: red;")),
#       if (failedValue)
#         div(tags$b("Invalid Attribute Value: must begin with printable character other than space", style = "color: red;")),
#       footer = tagList(
#         modalButton("Cancel"),
#         actionButton(ns("ok"), "Commit")
#       )
#     ) 
#   }
# }

#--------EDIT VALUE------------------------------
observeEvent(returnValue4ModuleTagVal$entryValue(),{
  if(rightPanel()=='tagValues'){
    # assuming tib is uptodate, simply work on the existing tib
    name<-returnValue4ModuleTagVal$name()
    #rowIndex<-returnValue4ModuleTagVal$rowIndex()
    if(!is.null(name) && 
       length(returnValue4ModuleTagVal$entryValue())>0 &&
       nchar(returnValue4ModuleTagVal$entryValue())>0 &&
       (getTibColumn()!=getTibPtColPos() 
    )  ){
      entry<-returnValue4ModuleTagVal$entryValue()
      # !!! TODO: type check if numeric
      #cat('entry=',entry,'\n')
      newPtDefs<-getPtDefs()
      name<-getTibName()
      column<-getTibColumn()
      row<-getTibRow()
      
      sender='applyTibEdit'
      newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
      updateAceExtDef(newPtDefs, sender=sender)
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





