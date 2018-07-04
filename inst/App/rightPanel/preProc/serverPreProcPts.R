
observeEvent( c(getRightMidPanel(), preProcDB$points) ,{
  cat('getRightMidPanel()=', getRightMidPanel(), "\n")
  cat('nrow=',nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName())),"\n")
  if(
    getRightMidPanel()=='point' && 
    nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName()))==0
  ){
    hideElement('sw-drop-PtPreProc-BadWolf')
  } else {
    showElement('sw-drop-PtPreProc-BadWolf')
  }
})


observeEvent(input$dilbert,{
  cat('input$dilbert', input$dilbert, "\n")
  if(input$dilbert %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    cat( "input#dilbert=", input$dilbert,"\n")
    txt= getPreProcPtScript()[input$dilbert]
    selectedAsset$ptScriptSel<-input$dilbert
    cat("updating catbert:")
    updateAceEditor(session, editorId='catberg', value=txt)
  }
 
})


observeEvent( input$commitPtPreProc,{
  cat("\n******input$commitPPP************\n")
  cat("input$dilbert=",input$dilbert,"\n")
  
  if(input$dilbert %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    newScript=input$catberg
    selectedAsset$ptScriptSel<-input$dilbert
   
    cmd<-input$dilbert
    setPreProcPtScript(
      tab_Id=getTibTabId(),
      tib_Name=getAssetName(),
      pt_Column_Name=getTibColumnName(),
      cmd_name=cmd,
      newScript=input$catberg
    )
    
  }
}, ignoreNULL = TRUE)
