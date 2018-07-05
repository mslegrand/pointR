
observeEvent( c(getRightMidPanel(), preProcDB$points) ,{
  # cat('getRightMidPanel()=', getRightMidPanel(), "\n")
  # cat('nrow=',nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName())),"\n")
  if(
    getRightMidPanel()=='point' && 
    nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName()))==0
  ){
    hideElement('sw-drop-PtPreProc-BadWolf')
  } else {
    showElement('sw-drop-PtPreProc-BadWolf')
  }
})


observeEvent(input$ptPreProcCmdChoice,{
  # cat('input$ptPreProcCmdChoice', input$ptPreProcCmdChoice, "\n")
  if(input$ptPreProcCmdChoice %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    # cat( "input#dilbert=", input$dilbert,"\n")
    txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    # cat("updating catbert:")
    updateAceEditor(session, editorId='catberg', value=txt)
  }
 
})


observeEvent( input$commitPtPreProc,{
  cat("\n******input$commitPPP************\n")
  cat("input$dilbert=",input$ptPreProcCmdChoice,"\n")
  cmd<-input$ptPreProcCmdChoice
  if(cmd %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    newScript=input$catberg
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    setPreProcPtScript(
      tab_Id=getTibTabId(),
      tib_Name=getAssetName(),
      pt_Column_Name=getTibColumnName(),
      cmd_name=cmd,
      newScript=input$catberg
    )
    
  }
}, ignoreNULL = TRUE)
