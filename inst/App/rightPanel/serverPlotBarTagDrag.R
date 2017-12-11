
# --------------input$plotNavBar=="dragTag"---------------- 

 # output$TagDragPanel<-renderUI({
 #  conditionalPanel( "input.plotNavBar=='tagDrag'", moduleTagDragUI("tagDragBar"))
 # })

returnValue4ModuleTagDrag<-callModule(
  module=moduleTagDrag,
  id="tagDragBar",
  barName=rightPanel,
  name=getTibName,
  nameChoices=getTibNameChoices,
  rowIndex=getTibRow,
  rowIndexChoices=getTibRowChoices,
  matColIndex=getTibMatCol,
  matColIndexChoices=getTibMatColChoices
)


observeEvent( returnValue4ModuleTagDrag$name() ,{
  if(rightPanel()=="tagDrag"){
    name<-returnValue4ModuleTagDrag$name()
    if(!is.null(name)){
      updateSelected(name=returnValue4ModuleTagDrag$name())
    }
  }
})

observeEvent( returnValue4ModuleTagDrag$rowIndex(), {
  if(rightPanel()=="tagDrag"){
    rowIndex<-returnValue4ModuleTagDrag$rowIndex()
    if(!is.null(rowIndex)){
      rowIndex=as.integer(rowIndex)
      updateSelected(row=rowIndex, matCol='end')
    }
  }
})

observeEvent(
  returnValue4ModuleTagDrag$tagClone(),
  {
    if(rightPanel()=="tagDrag"){
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
  }
)

observeEvent(
  returnValue4ModuleTagDrag$tagDelete(),
  {
    if(rightPanel()=="tagDrag"){
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
  }
)

observeEvent( returnValue4ModuleTagDrag$tagMoveUp(),{ 
    if(rightPanel()=="tagDrag"){
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
    }
})

observeEvent( returnValue4ModuleTagDrag$tagMoveDown(),{ 
  if(rightPanel()=="tagDrag"){
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
  }
})



#-------------------------------------------


  showPts.dragTag %<c-% function(
    ptName=NULL, 
    pts=NULL, 
    rowIndex=NULL,
    ptDisplayMode 
    ){
    onMouseDownTxt="ptRPlotter_ptR_SVG_TagDrag.selectElement(evt)" 
    
    
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


#-------------SVG-------------------------------------
# 
# output$svgTagDragPanel<-renderUI({
#   conditionalPanel( "input.plotNavBar=='tagDrag'", modulePlotSVGrUI("svgTagDragMod"))
# })

statusPlotTagDrag<-callModule(
  module=modulePlotSVGr,
  id="svgTagDragMod",
  svgID='ptR_SVG_TagDrag',
  showPts.compound=reactive({
    showPts.dragTag(
      ptName=getTibName(), 
      pts=getTibPts(), #matrix(unlist(getPtDefs()$tib[[getTagName()]]),2) ,
      rowIndex=getTibRow(),
      ptDisplayMode=getDisplayModeTag() #, 
      #tags=getTagIndexChoices()
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "TagDrag") }), # reactive({ js.scripts[[ "TagDrag"]] }),
  getSVGWH=getSVGWH,
  showGrid=showGrid,
  getCode= getCode, #srcGet,
  getCode2 =getCode, #srcGet,  # (or getCodeTransform)
  getErrorMssg=getErrorMssg,
  insert.end=",showPts.compound()"
)

observeEvent(statusPlotTagDrag$status(), {
  status<-statusPlotTagDrag$status()
  if(status$state!="PASS"){
    updateRightPanel('logPanel')
    mssg$err<-status$message    # send mssg to log
    # switch to log 
  }
})


