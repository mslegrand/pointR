output$uiPreProcChooser<-renderUI({
  radioGroupButtons(
    inputId = "ptPreProcCmdChoice",
    label = "Action",
    choices = preprocChoices #,
    #selected='onNewPt'
  )
})

observeEvent(input$ptPreProcCmdChoice, {
  if((input$ptPreProcCmdChoice %in% preprocChoices) &&  getRightMidPanel()%in% c('point', 'matrix') ){
    txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }
})

#kludge to have button inside UI, so splitter window is not disturbed.
onclick("commitPtPreProcRequest", click('commitPtPreProc') )

observeEvent( input$commitPtPreProc,{
  if(getRightMidPanel() %in% c('point', 'matrix')){
    # cat("\n******input$commitPtPreProc************\n")
    cmd<-input$ptPreProcCmdChoice
    if(cmd %in% preprocChoices){
      newScript=input$ptPreProcAceEditor
      selectedAsset$ptScriptSel<-cmd
      setPreProcPtScript(
        tab_Id=getTibTabId(),
        tib_Name=getAssetName(),
        pt_Column_Name=getTibColumnName(),
        cmd_name=cmd,
        newScript=newScript
      )
    }
  }
}, ignoreNULL = TRUE)
