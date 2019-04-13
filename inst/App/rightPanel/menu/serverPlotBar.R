#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    return(NULL)
  }
  
  if(cmd == 'cmdShowGrid'){
    renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    setSvgGrid(input$pages, show=TRUE)
  }
  
  if(cmd == 'cmdHideGrid'){
    renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    setSvgGrid(input$pages, show=FALSE)
  }
  
  if(cmd == 'cmdAdjustGridSpacing'){
    spacingChoices<-c(.01, .05, .1, .5 ,1, 5,50,100,500)
    sgrid<-getSvgGrid()
    choiceDX<-sgrid$dx
    choiceDY<-sgrid$dy
    modalGridSpacing <- function() {
      modalDialog(
            selectInput( "selectGridDX", "Horizontal Spacing", spacingChoices,
              multiple=FALSE, selectize = FALSE, width="90px", selected=choiceDX
            ),
            selectInput( "selectGridDY", "Vertical Spacing", spacingChoices,
              multiple=FALSE, selectize = FALSE, width="90px", selected=choiceDY
            ),
        footer = tagList(actionButton("modalGridSpacingCancel", "Cancel"),actionButton("modalGridSpacingOk", "OK") )
      ) 
    }
    showModal( modalGridSpacing() )
    dirtyDMDM(session, "plotNavBar")
  }

  if(cmd == 'cmdBackDropColor'){
    # popup color picker
  }  
  
  if(cmd == 'cmdHideBack'){
    setBackDrop(hide=FALSE)
  } 
  
  if(cmd == 'cmdShowPointsNoLabels'){
    disableDMDM(session,  menuBarId="plotNavBar", entry="cmdShowPointsNoLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdShowPointLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdHidePoints")
    setDisplayOption(ptMode='Normal')
  }
  
  if(cmd == 'cmdShowPointLabels'){
    enableDMDM(session,  menuBarId="plotNavBar", entry="cmdShowPointsNoLabels")
    disableDMDM(session,  menuBarId="plotNavBar",  entry="cmdShowPointLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdHidePoints")
    setDisplayOption(ptMode='Labeled')
  }
  
  if(cmd == 'cmdNewColumn'){
    showModal( addNewColModal() )
  }
  
  # if(cmd == 'cmdSetMatColMax'){
  #   columnName<-getTibColumnName()
  #   currentValue<-getPointMax()
  #   if(is.null(currentValue)){
  #     currentValue=NA
  #   } 
  #   showModal( setMatColMaxModal(columnName, currentValue) )
  # }
  
  # if(cmd == 'cmdDeleteColumn'){
  #   columnName<-getTibColumnName()
  #   showModal(deleteColumnModal(columnName))
  # }
  
  if(cmd == 'cmdNewPP'){ # disable unless ...
    columnName<-getTibColumnName()
    if( getRightMidPanel()=='point' && 
      nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName()))==0
    ){
      insertPreProcPtEntry(getTibTabId(), getAssetName(), getTibColumnName() )
      txt=fileTemplates[['newPtTemplate.R']]
      updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
    }
    dirtyDMDM(session, "plotNavBar")
  }
  
  if(cmd == 'cmdImportPP'){ # disable unless ...
      cmdPreProcPtsImport()
      dirtyDMDM(session, "plotNavBar")
  }  
  if(cmd=="cmdExportPP"){ #-----save
    cmdPreProcPtsExport()
    dirtyDMDM(session, "plotNavBar")
  }   
  if(cmd=="cmdRemovePP"){ #-----save
    cmdPreProcPtsRemove()
    dirtyDMDM(session, "plotNavBar")
  } 
  
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
  
})


