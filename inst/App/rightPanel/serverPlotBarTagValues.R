

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
  getTibEntryChoices=getTibEntryChoices,
  headerId=NS("tagValBar", 'header')
)

#name, rowIndex
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$rowIndex()),{
  if(rightPanel()=='tagValues'){
    cat('\n----Entering-----------oE 2-123\n')
    name<-returnValue4ModuleTagVal$name()
    if(is.null(name)){cat('name is null\n')} else { 
      cat('name=',name,'\n')
      cat('nchar(name)=',nchar(name),'\n')
    }
    rowIndex<-returnValue4ModuleTagVal$rowIndex()
    tib<-name %AND% getPtDefs()$tib[[name]]
    ### todo 
    # 1. bail unless rowIndex <= nrow(tib)
    # 2. currentEntry<-  
    colName<-getTibColumnName()
    #browser()
    indices<-extractSafeRowColIndex(tib, rowIndex, colName)
    if(!is.null(indices)){
      entry<-tib[[indices$rowIndex, indices$colIndex]]
      if( is.matrix(entry) && dim(entry)[1]==2 ){
        matColIndex<-ncol(entry)
        updateSelected(name=name, row=rowIndex, matCol=matColIndex)
      } else {
        updateSelected(name=name, row=rowIndex)
      }
    }
    
    
    
    # 2 if not tibEdit.point
    #    2.1 update now
    # 2. if tibEdit.point point
    #    2.1 get entry m
    #    2.2 bail if n is empty?  
    #    2.2 compute ncol(m)
    #    compute Max matColIndex at that row
    #    and update with the constraint of being <= ncol(m)
    
      
    # pointCol<-colName %AND% tib %AND% tib[[colName ]]
    # 
    # if( !is.null(pointCol) && !is.null(rowIndex) ){
    #   if(1<=rowIndex && rowIndex<=length(pointCol)){
    #     m<-pointCol[[rowIndex]]
    #     if('matrix' %in% class(m)){
    #       cat('----updateSelected matCol-----------oE 2-123\n')
    #       matColIndex<-ncol(m)
    #       cat('matColIndex=', matColIndex,"\n")
    #       updateSelected(name=name, row=rowIndex, matCol=matColIndex)
    #     }
    #   }
    # }
      #pts<-newTib[[getTibPtColPos()]]
    cat('----Exiting-----------oE 2-123\n')
  }
  
  }
)
 
#name, columnName
observeEvent(c(returnValue4ModuleTagVal$name(),returnValue4ModuleTagVal$columnName()),{
  if(rightPanel()=='tagValues'){
    cat('\n-----Entering---------------\noE 2-124\n')
    name<-returnValue4ModuleTagVal$name()
    colName<-returnValue4ModuleTagVal$columnName()
    cat('colName==',colName,"\n")
    if(!is.null(colName) && nchar(colName)>0){
      # columnNameChoices=getTibColumnNameChoices()
      # ptPos<-getTibPtColPos()
      # column<-match(colName, columnNameChoices, nomatch=ptPos)
      # cat('oE 2-124 columnName=', colName, "\n")
      # cat('oE 2-124 columnNameChoices=\n')
      # print(columnNameChoices)
      # cat('oE 2-124 setting column index=', column, "\n")
      updateSelected(columnName=colName)
    }
  }
})




#--------EDIT VALUE------------------------------
observeEvent(returnValue4ModuleTagVal$entryValue(),{
  if(rightPanel()=='tagValues'){
    cat('\n--------Entering---------entryVAlue\n')
    cat('oE 2-125\n')
    
    # assuming tib is uptodate, simply work on the existing tib
    name<- returnValue4ModuleTagVal$name() 
    entry<-name %AND% returnValue4ModuleTagVal$entryValue()
    
    cat('class(name)=',class(name),'\n')
    
    #rowIndex<-returnValue4ModuleTagVal$rowIndex()
    row= entry %AND% returnValue4ModuleTagVal$rowIndex()
    columnName<-row %AND% returnValue4ModuleTagVal$columnName()
    cat('row=',row,"\n")
    cat("columnName=" ,columnName,"\n")
    
    cat('entry=',entry,'\n')
    
    if(!is.null(columnName) && 
       #length(entry)>0 &&
       nchar(entry)>0   ){
      #entry<-returnValue4ModuleTagVal$entryValue()
      # !!! TODO: type check if numeric
      rP<-rightPanel()
      if(is.null(rP))rP<-'NULL'
      cat('rightPanel=',rP,"\n")
      cat('class(entry)=',class(entry),"\n")
      cat('entry=',entry,"\n")
      cat('setting entry state\n')
      setPlotState(entry)
      if(!(entry %in% c('matrix','point'))){
        cat('inside entry state\n')
        name<-getTibName()
        newPtDefs<-getPtDefs()
        column<-getTibColumnName()
        row<-newPtDefs$tib[[name]] %AND% getTibRow()
        if(!is.null(row) && row>=1 && row<=nrow(newPtDefs$tib[[name]]) ){
          sender='applyTibEdit'
          newPtDefs$tib[[getTibName()]][[row,column ]]<-entry
          updateAceExtDef(newPtDefs, sender=sender)
          updateSelected( name=name, columnName=columnName, row=row)
        }
      } 
    }
    isolate({
      cat('--------Exiting---------entryVAlue\n')
    })
  }
})

observeEvent(
  returnValue4ModuleTagVal$tagClone(),
  {
    cat('oE 2-123\n')
    #if(rightPanel()=="tagDrag"){
      sender='cloneRow'
      ptDefs<-getPtDefs()
      name<-getTibName()
      tib<-ptDefs$tib[[name]]
      rowIndex<-getTibRow()
      newTib<-bind_rows(tib[1:rowIndex,], tib[rowIndex:nrow(tib),])
      rowIndx=rowIndex+1
      matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      
      pts<-newTib[[getTibPtColPos()]]
      
      ptDefs$tib[[name]]<-newTib
      newPtDefs<-ptDefs
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(row=rowIndex, matCol=matCol)
    }
  #}
)

observeEvent(
  returnValue4ModuleTagVal$tagDelete(),
  {
    #if(rightPanel()=="tagDrag"){
      sender='deleteRow'
      ptDefs<-getPtDefs()
      name<-getTibName()
      newTib<-ptDefs$tib[[name]]
      rowIndex<-getTibRow()
      
      # !!!TODO handle case where this would be last row.
      newTib<-newTib[-rowIndex,]
      ptDefs$tib[[name]]<-newTib
      newPtDefs<-ptDefs
      
      #adjust position
      rowIndex<-min(rowIndex, nrow(newTib))
      matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      if(length(matCol)==0){matCol=0}
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(row=rowIndex, matCol=matCol)
    }
  #}
)

observeEvent( returnValue4ModuleTagVal$tagMoveUp(),{ 
  #if(rightPanel()=="tagDrag"){
    rowIndex<-getTibRow()
    if(rowIndex>1){
      sender='tagMoveUp'
      ptDefs<-getPtDefs()
      name<-    getTibName()
      newTib<-ptDefs$tib[[name]]
      
      newTib[c(rowIndex,rowIndex-1),]<-newTib[c(rowIndex-1,rowIndex),]
      ptDefs$tib[[name]]<-newTib
      newPtDefs<-ptDefs
      
      #adjust position
      rowIndex<-rowIndex-1
      matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      if(length(matCol)==0){matCol=0}
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(row=rowIndex, matCol=matCol)   
    }
  #}
})

observeEvent( returnValue4ModuleTagVal$tagMoveDown(),{ 
  #if(rightPanel()=="tagDrag"){
    rowIndex<-getTibRow()
    ptDefs<-getPtDefs()
    name<-    getTibName()
    newTib<-ptDefs$tib[[name]]
    if(rowIndex<nrow(newTib)){
      sender='tagMoveDown'
      
      newTib[c(rowIndex,rowIndex+1),]<-newTib[c(rowIndex+1,rowIndex),]
      ptDefs$tib[[name]]<-newTib
      newPtDefs<-ptDefs
      
      #adjust position
      rowIndex<-rowIndex+1
      matCol<-ncol(newTib[[rowIndex, getTibPtColPos()]])
      if(length(matCol)==0){matCol=0}
      updateAceExtDef(newPtDefs, sender=sender)
      updateSelected(row=rowIndex, matCol=matCol)   
    }
  #}
})


#-------points----------------------------------------------

#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( returnValue4ModuleTagVal$removePt(), {
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
observeEvent( returnValue4ModuleTagVal$tagPt(), {
  
  #if(rightPanel()=="Points"){
    #selection<-input$ptRSelect
    cat("Enter tagPt\n")
    src<-getCode() 
    selection<-getTibName()
    ptDefs<-getPtDefs()
    
    row=getTibRow() 
    matCol=getTibMatCol() 
    
    m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
    if(ncol(m)<1){ 
      return(NULL) # bail if matrix of points is empty
    }
    tib<-ptDefs$tib[[selection]] #get the tib 
    tib<-tagTib(tib, getTibPtColPos(), row, matCol)
    row<-row+1
    matCol<-length(tib[[row, getTibPtColPos()]])/2
    ptDefs$tib[[selection]]<-tib 
    sender='tagPt'
    updateAceExtDef(ptDefs, sender=sender)
    updateSelected(row=row, matCol=matCol)
  #} #end of if
}) #end of point InfoList Tag Point, 


observeEvent( returnValue4ModuleTagVal$forwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length( matColIndex)>0 && length(matColChoices)>0){
    cat("observeEvent:: serverPlotBar 99\n")
    matColIndex=max(matColIndex+1, min(matColChoices) )
    updateSelected(  matCol=matColIndex )
  }
})

observeEvent( returnValue4ModuleTagVal$backwardPt(), {
  matColIndex<-getTibMatCol()
  matColChoices<-getTibMatColChoices()
  if(length(matColIndex)>0 && length(matColChoices)>0){
    cat("observeEvent:: serverPlotBar 98\n")
    matColIndex=max(matColIndex-1, min(matColChoices) )
    updateSelected(  matCol=matColIndex  )
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
  if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
  onMouseDownTxt<-"ptRPlotter_ptR_SVG_TagVal.selectElement(evt)"
  
  cat("rowIndx=", rowIndex, "\n")
  cat("length(ptName)=", length(ptName), "\n")
  cat("length(pts)=", length(pts), "\n")
  if(length(ptName)<1){return(NULL)}
  if(length(pts)<1)  {return(NULL) }
  
  
  
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
        cat('m is null\n')
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
      cat('length(mRow)=0\n')
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
    cat("statusPlotTagVal$status() ERRUR\n")
    updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    #mssg$err<-'TagValGraphErr'
    # switch to log 
  }
})

# observeEvent(statusPlotTagVal$status(), {
#   status<-statusPlotTagVal$status()
#   if(status$state!="PASS"){
#     updateRightPanel('logPanel')
#     mssg$err<-status$message    # send mssg to log
#     # switch to log 
#   }
# })
# 




