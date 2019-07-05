
getSVGWH<-reactiveVal(c(650,620)) 



observeEvent(input$ptPreProcDropDown,{
  updateAceEditor(session, editorId='ptPreProcAceonNewPt', value=getPreProcPtScript()["onNewPt"])
  updateAceEditor(session, editorId='ptPreProcAceonMovePt', value=getPreProcPtScript()["onMovePt"])
  updateAceEditor(session, editorId='ptPreProcAceonMoveMat', value=getPreProcPtScript()["onMoveMat"])
})

observeEvent(input$dimissPtPreProcButton,{
  click('ptPreProcDropDown')
})

output$ptPreProcSource<-renderText('Point Preprocessor')

observeEvent( input$commitPtPreProcButton,{ 
  if(getRightMidPanel() %in% c('point', 'matrix')){
    
    log.fin(input$commitPtPreProcButton)
    selectedAsset$ptScriptSel<-'onNewPt'
    
    for(cmd in c('onNewPt', "onMovePt","onMoveMat")){
      aceId=paste0("ptPreProcAce", cmd)
      newScript=input[[aceId]]
      setPreProcPtScript(
        tab_Id=getTibTabId(),
        tib_Name=getAssetName(),
        pt_Column_Name=getTibColumnName(),
        cmd_name=cmd,
        newScript=newScript
      )
    }
    log.fout(input$commitPtPreProcButton)
  }
}, ignoreNULL = TRUE) 

