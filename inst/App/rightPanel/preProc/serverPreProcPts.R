
getSVGWH<-reactiveVal(c(650,620)) 



observeEvent(input$ptPreProcDropDown,{
  preprocScripts<-getPreProcPtScript()
  # this is where we update the tabs and ace
  for(name in unlist(preprocChoices, use.names = FALSE)){
    if(name %in% names(preprocScripts)){
      editorId=paste0('ptPreProcAce', name)
      showTab("ptPreProcpages",name )
      updateAceEditor(session, editorId=editorId, value=preprocScripts[name])
    } else {
      hideTab("ptPreProcpages",name)
    }
  }
})

observeEvent(input$dimissPtPreProcButton,{
  click('ptPreProcDropDown')
})

output$ptPreProcSource<-renderText('Point Preprocessor')

observeEvent( input$commitPtPreProcButton,{ 
  log.fin(input$commitPtPreProcButton)
  preprocScripts<-getPreProcPtScript()
  # this is where we update the tabs and ace
  for(name in unlist(preprocChoices, use.names = FALSE)){
    if(name %in% names(preprocScripts)){
      editorId=paste0('ptPreProcAce', name)
      newScript=input[[editorId]]
      setPreProcPtScript(
        tab_Id=getTibTabId(),
        tib_Name=getAssetName(),
        pt_Column_Name=getTibColumnName(),
        cmd_name=name,
        newScript=newScript
      )
    } 
  }
  log.fout(input$commitPtPreProcButton)
  
  # if(getRightMidPanel() %in% c('point', 'matrix')){
  #   
  #   log.fin(input$commitPtPreProcButton)
  #   selectedAsset$ptScriptSel<-'onNewPt'
  #   
  #   for(cmd in c('onNewPt', "onMovePt","onMoveMat")){
  #     aceId=paste0("ptPreProcAce", cmd)
  #     newScript=input[[aceId]]
  #     setPreProcPtScript(
  #       tab_Id=getTibTabId(),
  #       tib_Name=getAssetName(),
  #       pt_Column_Name=getTibColumnName(),
  #       cmd_name=cmd,
  #       newScript=newScript
  #     )
  #   }
  #   log.fout(input$commitPtPreProcButton)
  # }
}, ignoreNULL = TRUE) 

