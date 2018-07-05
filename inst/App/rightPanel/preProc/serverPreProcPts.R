
observeEvent(input$ptPreProcCmdChoice,{
  if(input$ptPreProcCmdChoice %in% c( 'onNewPt', 'onMovePt',  'onMoveMat')){
    txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }
})

onclick("commitPtPreProcRequest", click('commitPtPreProc') )

observeEvent( input$commitPtPreProc,{
  cat("\n******input$commitPtPreProc************\n")
  cmd<-input$ptPreProcCmdChoice
  if(cmd %in% c( 'onNewPt', 'onMovePt',  'onMoveMat')){
    newScript=input$ptPreProcAceEditor
    cat('newScript=\n')
    cat(newScript)
    selectedAsset$ptScriptSel<-cmd
    setPreProcPtScript(
      tab_Id=getTibTabId(),
      tib_Name=getAssetName(),
      pt_Column_Name=getTibColumnName(),
      cmd_name=cmd,
      newScript=newScript
    )
  }
}, ignoreNULL = TRUE)
