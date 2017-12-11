

# --------------input$plotNavBar=="tagValues"---------------- 

returnValue4ModuleTagVal<-callModule(
  module=moduleTagVal,
  id="tagValBar",
  id2="tagValBar", # !!! DO  WE STILL NEED THIS???? 
  barName=rightPanel ,
  name=getTibName,
  nameChoices=getTibNameChoices,
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
      
    
      updateSelected(name=name, row=rowIndex, 
                     matCol=matColIndex)
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

#--------DELETE COLUMN------------------------------

deleteColumnModal <- function(columnName = NULL) {
  warningMssg<-paste('Warning: About to delete the column:', columnName) 
  modalDialog(
    span('DELETING!!!'), 
    if(!is.null(columnName)){
      div(
        tags$b(warningMssg, style = "color: red;")
      ) 
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("deleteColumnButton", "Delete")
    )
  ) 
}

observeEvent(returnValue4ModuleTagVal$deleteColumn(),{
  if(rightPanel()=='tagValues'){
    columnName<-getTibColumnName()
    # !!! Addchecks here
    showModal(deleteColumnModal(columnName))
  }
})

observeEvent(input$deleteColumnButton, {
  if(rightPanel()=='tagValues'){
    indx<-getTibColumn()
    newPtDefs<-getPtDefs()
    newPtDefs$tib[[getTibName()]]<-newPtDefs$tib[[getTibName()]][,-indx]
    sender<-'deleteColumn'
    updateAceExtDef(newPtDefs, sender=sender)
    # update columnSelection
    removeModal() #close dialog
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
  rowIndex=NULL,
  ptDisplayMode #,  
  #tags=NULL
  ){
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
 
  #cat("rowIndx=", rowIndex, "\n")
  if(length(ptName)<1){return(NULL)}
  if(length(pts)<2)  {return(NULL) }
  
  if(length(rowIndex)<1 || rowIndex==0){return(NULL)}
  
  semitransparent<-0.3
  colorScheme<-c(default="purple", ending="red", selected="blue")
  color<-colorScheme[1]
  
  
  
  
  opacity<-rep(semitransparent, length(pts)) 
  opacity[rowIndex]<-1 
  rowNums<-seq(length(pts))
  ids<-paste("pd",ptName,rowNums,sep="-")
  offRows<-rowNums[-rowIndex]
  mRow<-pts[[rowIndex]]
  
  list( 
    lapply(offRows, function(i){
      m<-pts[[i]]
      if(length(m)==0){
        NULL
      } else {
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
      }
    }),
    if(length(mRow)==0){
      NULL
    } else {
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
    }
  ) #end list
} #end showPts



statusPlotTagVal<-callModule(
  module=modulePlotSVGr,
  id="svgTagValsMod",
  svgID='ptR_SVG_TagVal',
  showPts.compound=reactive({
    showPts.valTag(
      ptName=getTibName(), 
      pts=getTibPts(), 
      
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayModeTag() #, 
      
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





