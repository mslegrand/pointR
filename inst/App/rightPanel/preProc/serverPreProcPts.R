
getSVGWH<-reactiveVal(c(650,620)) 


output$uiPreProcChooser<-renderUI({
  radioGroupButtons(
    inputId = "ptPreProcCmdChoice",
    label = "Action",
    choices = preprocChoices 
  )
})


# updates whenever either
#  input$pages changes  (input$pages contains the tabId of the current page)
#  selectedAsset$tabId changes
# ??? do we really need both of these  ??? !!!
observeEvent(c(input$pages, getTibTabId()), {
  if(identical(input$pages, getTibTabId())){
    if(hasPtScript()){
      choice<-input$ptPreProcCmdChoice
      if(is.null(choice)){ choice<-1}
      txt= getPreProcPtScript()[choice]
      updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
    } 
  }
})

# triggered whenever a choicetab changes occurs:
# This occurs either by the user changing the choice or by
# a page change and a null choice becoming 1.
observeEvent(input$ptPreProcCmdChoice, {
  if(
      input$ptPreProcCmdChoice %in% preprocChoices &&  
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

# !!! kludge to have button inside UI, so splitter window is not disturbed.
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
