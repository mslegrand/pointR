
getSVGWH<-reactiveVal(c(650,620)) 




# 
# observeEvent(input$ptPreProcDropDown,{
#   log.fin(input$ptPreProcDropDown )
#   # preprocScripts<-getPreProcPtScript()
#   # getPreProcScriptName(tab_Id, tib_Name, column_Name)
#   # this is where we update the tabs and ace
#   for(name in unlist(preprocChoices, use.names = FALSE)){
#     if(name %in% names(preprocScripts)){
#       editorId=paste0('ptPreProcAce', name)
#       showTab("ptPreProcpages",name )
#       updateAceEditor(session, editorId=editorId, value=preprocScripts[name])
#     } else {
#       hideTab("ptPreProcpages",name)
#     }
#   }
#   delay(1000, disable("commitPtPreProcButton"))
#   log.fout(input$ptPreProcDropDown )
# })

# observeEvent(input$dimissPtPreProcButton,{
#   click('ptPreProcDropDown')
# })


observeEvent(input$dimissPreProcChoiceButton,{
  click('preProcDropDown')
})


# lapply(unlist(preprocChoices, use.names = FALSE), function(name){
#   editorId=paste0('ptPreProcAce', name)
#   observeEvent( input[[editorId]], {
#     enable("commitPtPreProcButton")
#   }, ignoreInit = TRUE, ignoreNULL = TRUE)
# })



output$ptPreProcSource<-renderText(
  if(identical(getColumnType(),'point')){
    'Point Preprocessor'
  } else {
    'Attribute Value Preprocessor'
  }
)

# observeEvent( input$commitPtPreProcButton,{ 
#   
#   preprocScripts<-getPreProcPtScript() # retrieves scripts from db
#   # this is where we update the tabs and ace
#   for(name in unlist(preprocChoices, use.names = FALSE)){ #runs over all preproc cmds
#     if(name %in% names(preprocScripts)){ # restricts to scripts existing in db
#       editorId=paste0('ptPreProcAce', name)
#       newScript=input[[editorId]]
#       
#         setPreProcPtScript(
#           tab_Id=getTibTabId(),
#           tib_Name=getAssetName(),
#           pt_Column_Name=getTibColumnName(),
#           cmd_name=name,
#           newScript=newScript
#         )
#       
#       
#     } 
#   }
#   disable("commitPtPreProcButton")
#   # log.fout(input$commitPtPreProcButton)
# }, ignoreNULL = TRUE)

observeEvent( input$commitPreProcChoiceButton, {
  scriptName=input$preProcChooser
  setPreProcScriptName(
    tab_Id=getTibTabId(),
    tib_Name=getAssetName(),
    column_Name=getTibColumnName(),
    script_Name=scriptName
  )
  click('preProcDropDown')
}, ignoreNULL = TRUE)
