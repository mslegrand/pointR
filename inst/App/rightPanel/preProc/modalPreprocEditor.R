cmdPreProcEdit<-function(preprocScripts, preprocName){
  showModal( 
    modalPreProcEditor(preprocScripts=preprocScripts, preprocName=preprocName) 
  )
}

# creates a single tabPanel entry
newPreProcPanel<-function(label, value){
  outputId<-paste0('ptPreProcAce',label)
  tabPanel(label,
           div(
             aceEditor(
               outputId=outputId,
               height = "380px",
               mode='r',
               value=value
             )
           )
  )
}

preProcTabSetPanel<-function(id='ptPreProcpages', preprocScripts ){
  preprocScripts<-unlist(preprocScripts, use.names = FALSE)
  pptabs<-mapply(newPreProcPanel, names(preprocScripts), preprocScripts, 
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  do.call(tabsetPanel, pptabs)
}


# dropdown for preprocessor

modalPreProcEditor <- function( preprocScripts, preprocName, failed=0, mssg=' '  ) {
  modalDialog(
    div( id='ptPreProcBackPanel', class='backPanel', style='padding:20px;',
        #div( style='margin-left:20px; color: #00ffff;', h4(textOutput("ptPreProcSource")) ) ,
        textInput("modalPreprocName", 
                  label=span(style='color: #00ffff;', 'Preprocessor Name'),  
                  value=preprocName, 
                  width='100%', 
                  placeholder = 'enter name for preproc with at least 8 characters'
        ),
        preProcTabSetPanel(id='ptPreProcpages', preprocScripts=preprocScripts)
    ),
    title="Preproc Editor",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("modalPreprocEditorCommitOk", "OK")
    )
  )
}


observeEvent( input$modalPreprocEditorCommitOk,{
  browser()
  preprocName<-input$modalPreprocName
  if(nchar(preprocName)<0){
    
  } else {
    removeModal()
  }
})



# for(name in unlist(preprocChoices, use.names = FALSE)){
#   if(name %in% names(preprocScripts)){
#     editorId=paste0('ptPreProcAce', name)
#     showTab("ptPreProcpages",name )
#     updateAceEditor(session, editorId=editorId, value=preprocScripts[name])
# 
# 

