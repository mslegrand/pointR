output$uiPreProcChooser<-renderUI({
  radioGroupButtons(
    inputId = "ptPreProcCmdChoice",
    label = "Action",
    choices = preprocChoices #,
    #selected='onNewPt'
  )
})

observeEvent(c(input$pages, getTibTabId()), {
  if(identical(input$pages, getTibTabId())){
    if(hasPtScript()){
      #browser()
      txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
      updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
    }
  }

})

observeEvent(input$ptPreProcCmdChoice, {
  if(
      (input$ptPreProcCmdChoice %in% preprocChoices) &&  
      getRightMidPanel()%in% c('point', 'matrix') 
  ){
    txt= getPreProcPtScript()[input$ptPreProcCmdChoice]
    # cat("serverPreProcPts.R:: input$ptPreProcCmdChoice\n")
    # cat( "input$ptPreProcCmdChoice=",format(input$ptPreProcCmdChoice),"\n")
    # cat("txt=",format(txt),"\n" )
    selectedAsset$ptScriptSel<-input$ptPreProcCmdChoice
    updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }
}, label="serverPreProcPts.R:: input$ptPreProcCmdChoice")

#kludge to have button inside UI, so splitter window is not disturbed.
onclick("commitPtPreProcRequest", click('commitPtPreProc') )

observeEvent( input$commitPtPreProc,{
  if(getRightMidPanel() %in% c('point', 'matrix')){
    # cat("\n******input$commitPtPreProc************\n")
    cmd<-input$ptPreProcCmdChoice
    if(cmd %in% preprocChoices){
      # cat('cmd=',format(cmd),"\n")
      newScript=input$ptPreProcAceEditor
      # cat(format(newScript),"\n")
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
