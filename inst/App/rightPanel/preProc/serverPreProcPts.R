
# observeEvent( c(getRightMidPanel(), preProcDB$points) ,{
#   # cat('getRightMidPanel()=', getRightMidPanel(), "\n")
#   # cat('nrow=',nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName())),"\n")
#   if(
#     getRightMidPanel()=='point' && 
#     nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName()))==0
#   ){
#     hideElement('sw-drop-PtPreProc-BadWolf')
#   } else {
#     showElement('sw-drop-PtPreProc-BadWolf')
#   }
# })


observeEvent(input$ptPreProcCmdChoice,{
  # cat('input$ptPreProcCmdChoice', input$ptPreProcCmdChoice, "\n")
  if(input$ptPreProcCmdChoice %in% c( 'onNewPt', 'onMovePt',  'onMoveMat')){
    # cat( "input#ptPreProcCmdChoice=", input$ptPreProcCmdChoice,"\n")
    txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    # cat("updating ptPreProcAceEditor:")
    updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }
})

onclick("commitPtPreProcRequest", click('commitPtPreProc') )

observeEvent( input$commitPtPreProc,{
  cat("\n******input$commitPtPreProc************\n")
  cmd<-input$ptPreProcCmdChoice
  cat("cmd=",input$ptPreProcCmdChoice,"\n")

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
