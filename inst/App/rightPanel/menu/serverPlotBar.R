#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    # cat('input$plotNavBar:: getRightMenuCmd()=NULL\n')
    return(NULL)
  }
  # if('character' %in% class(cmd)){
  #   cat('serverPlotBar::  input$plotBar  cmd=',cmd,'\n')
  # }
  
  if(cmd == 'cmdShowGrid'){
    renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    setDisplayOption(showGrid=TRUE)
  }
  if(cmd == 'cmdHideGrid'){
    renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    setDisplayOption(showGrid=FALSE)
  }
  
  if(cmd == 'cmdHidePoints'){
    renameDMDM(session,  "plotNavBar", "cmdHidePoints", "Hide Points", newValue="cmdShowPoints")
    setDisplayOption(ptMode='Hidden')
  }
  if(cmd == 'cmdShowPoints'){
    renameDMDM(session,  "plotNavBar", "cmdShowPoints", "Show Points", newValue="cmdHidePoints")
    setDisplayOption(ptMode='Normal')
  }
  if(cmd == 'cmdHidePointLabels'){
    renameDMDM(session,  "plotNavBar", "cmdHidePointLabels", "Hide  Labels", newValue="cmdShowPointLabels")
    setDisplayOption(ptMode='Normal')
  }
  if(cmd == 'cmdShowPointLabels'){
    renameDMDM(session,  "plotNavBar", "cmdShowPointLabels", "Show  Labels", newValue="cmdHidePointLabels")
    setDisplayOption(ptMode='Labeled')
  }
  if(cmd == 'cmdNewColumn'){
    showModal( addNewColModal() )
  }
  if(cmd == 'cmdSetMatColMax'){
    columnName<-getTibColumnName()
    currentValue<-getHandlerValue()
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


