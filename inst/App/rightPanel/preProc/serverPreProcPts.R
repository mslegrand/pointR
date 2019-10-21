
getSVGWH<-reactiveVal(c(650,620)) 

observeEvent(input$dimissPreProcChoiceButton,{
  click('preProcDropDown')
})

output$ptPreProcSource<-renderText(
  if(identical(getColumnType(),'point')){
    'Point Preprocessor'
  } else {
    'Attribute Value Preprocessor'
  }
)

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
