
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

observeEvent( selectedAsset$ptAddScript,{
  if(identical(input$dilbert, "onNewPt")){
    # set 'catberg' value to input$dilbert
    cat("\n\n\n**********selectedAsset$ptAddScript**********\n\n")
    updateAceEditor(session, editorId='catberg', value=selectedAsset$ptAddScript)
  }
}, ignoreNULL = TRUE)

observeEvent( selectedAsset$ptMoveScript,{
  if(identical(input$dilbert, "onMovePt")){
    # set 'catberg' value to input$dilbert
    cat("\n\n\n**********selectedAsset$ptMoveScript**********\n\n")
    updateAceEditor(session, editorId='catberg', value=selectedAsset$ptMoveScript)
  }
}, ignoreNULL = TRUE)

observeEvent( selectedAsset$ptDeleteScript,{
  if(identical(input$dilbert, "onDeletePt")){
    # set 'catberg' value to input$dilbert
    cat("\n\n\n**********selectedAsset$ptDeleteScript**********\n\n")
    updateAceEditor(session, editorId='catberg', value=selectedAsset$ptDeleteScript)
  }
}, ignoreNULL = TRUE)

observeEvent(input$dilbert,{
  cat('input$dilbert', input$dilbert, "\n")
  if(input$dilbert %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    txt= list(
    onNewPt=selectedAsset$ptAddScript,
    onMovePt= selectedAsset$ptMoveScript,
    onDeletePt= selectedAsset$ptDeleteScript
  )[[input$dilbert]]
  updateAceEditor(session, editorId='catberg', value=txt)
  }
 
})


observeEvent( input$commitPtPreProc,{
  cat("\n******input$commitPPP************\n")
  cat("input$dilbert=",input$dilbert,"\n")
  if(identical(input$dilbert, "onNewPt")){
    # set 'catberg' value to input$dilbert
    selectedAsset$ptAddScript<-input$catberg
  } else if ( identical(input$dilbert, "onMovePt") ){
    selectedAsset$ptMoveScript<-input$catberg
  } else if( identical(input$dilbert, "onDeletePt") ){
    selectedAsset$ptDeleteScript<-input$catberg
  }
  if(input$dilbert %in% c( 'onNewPt', 'onMovePt',  'onDeletePt')){
    
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
