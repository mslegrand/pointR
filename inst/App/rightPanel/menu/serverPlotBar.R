#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    return(NULL)
  }
  
  if(cmd == 'cmdShowGrid'){
    renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    setDisplayOption(showGrid=TRUE)
  }
  
  if(cmd == 'cmdHideGrid'){
    renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    setDisplayOption(showGrid=FALSE)
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
  
  if(cmd == 'cmdSetMatColMax'){
    columnName<-getTibColumnName()
    currentValue<-getPointMax()
    if(is.null(currentValue)){
      currentValue=NA
    } 
    showModal( setMatColMaxModal(columnName, currentValue) )
  }
  
  if(cmd == 'cmdDeleteColumn'){
    columnName<-getTibColumnName()
    showModal(deleteColumnModal(columnName))
  }
  
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
  
})


